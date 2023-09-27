{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Web.WebPush (
-- * Functions
  generateVAPIDKeys
, vapidPublicKeyBytes
, sendPushNotification
, sendPushNotifications
, pushExpireInSeconds
, pushMessage
, mkPushNotification
-- * Types
, Subscription(..)
, VapidConfig(..)
, VAPIDKeys
, VAPIDKeysMinDetails(..)
, PushNotification
, PushNotificationCreated(..)
, PushNotificationError(..)
, PushP256dh
, PushAuth
) where

import           Web.WebPush.Internal

import           Control.Exception          (Exception, try)
import           Control.Exception.Base     (SomeException (..), fromException)
import           Control.Exception.Safe     (tryAny)
import           Control.Lens               (Lens, Lens', lens, (^.))
import           Control.Monad.Except
import qualified Crypto.PubKey.ECC.DH       as ECDH
import qualified Crypto.PubKey.ECC.ECDSA    as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Types    as ECC
import           Crypto.Random              (MonadRandom (getRandomBytes))
import qualified Data.Aeson                 as A
import           Data.Bifunctor
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64.URL as B64.URL
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.List                  as L
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Read             as TR
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Data.Word                  (Word8)
import           GHC.Int                    (Int64)
import           Network.HTTP.Client        (HttpException (HttpExceptionRequest),
                                             HttpExceptionContent (StatusCodeException),
                                             Manager, RequestBody (..),
                                             Response (..), httpLbs, method,
                                             requestBody, requestFromURI,
                                             requestHeaders, responseStatus)
import           Network.HTTP.Types         (Header, hContentEncoding,
                                             hContentType)
import           Network.HTTP.Types.Status  (Status (statusCode))
import           Network.URI
import           System.Random              (randomRIO)

-- | Generate the 3 integers minimally representing a unique pair of public and private keys.
--
-- Store them securely and use them across multiple push notification requests.
generateVAPIDKeys :: MonadRandom m => m (Either String VAPIDKeysMinDetails)
generateVAPIDKeys = do
  -- SEC_p256r1 is the NIST P-256
  (pubKey, privKey) <- ECC.generate $ ECC.getCurveByName ECC.SEC_p256r1
  case ECDSA.public_q pubKey of
    ECC.PointO -> pure $  Left "Invalid public key generated, public_q is the point at infinity"
    ECC.Point pubX pubY ->
      pure $ Right VAPIDKeysMinDetails {
          privateNumber = ECDSA.private_d privKey
        , publicCoordX = pubX
        , publicCoordY = pubY
      }

-- | Pass the VAPID public key bytes to browser when subscribing to push notifications.
-- Generate the application server key in a browser using:
--
-- > applicationServerKey = new Uint8Array( #{toJSON vapidPublicKeyBytes} )
vapidPublicKeyBytes :: VAPIDKeysMinDetails -> [Word8]
vapidPublicKeyBytes keys = BS.unpack $ ecPublicKeyToBytes' (x, y)
  where
    x = publicCoordX keys
    y = publicCoordY keys

-- | Result of creating a push notification
data PushNotificationCreated = PushNotificationCreated {
  pushNotificationCreatedTTL :: Maybe Int -- ^ Optional TTL of the notification
} deriving (Eq, Ord, Show)

-- | 3 integers minimally representing a unique VAPID public-private key pair.
data VAPIDKeysMinDetails = VAPIDKeysMinDetails { privateNumber :: Integer
                                               , publicCoordX  :: Integer
                                               , publicCoordY  :: Integer
                                               } deriving (Show)

-- | 
data VapidConfig = VapidConfig {
  vapidConfigContact :: T.Text
, vapidConfigKey :: VAPIDKeysMinDetails
}

sendPushNotifications :: (MonadIO m, A.ToJSON msg, MonadRandom m)
                      => Manager
                      -> VapidConfig
                      -> PushNotification msg
                      -> [Subscription]
                      -> m [(Subscription, Either PushNotificationError PushNotificationCreated)]
sendPushNotifications httpManager vapidConfig pushNotification subscriptions = do
  fmap concat $ forM (Map.toList subscriptionsMap) $ \(host, hostSubscriptions) -> do
    time <- liftIO getPOSIXTime
    let serverIdentification = ServerIdentification {
              serverIdentificationAudience = host
            , serverIdentificationExpiration = round time + fromIntegral (pushNotification ^. pushExpireInSeconds)
            , serverIdentificationSubject = vapidConfigContact vapidConfig
          }
    headers <- hostHeaders vapidKeys serverIdentification
    forM hostSubscriptions $ \subscription -> do
      e <- sendPushNotification' vapidKeys httpManager headers pushNotification subscription
      pure (subscription, e)
  where
    vapidKeysMin = vapidConfigKey vapidConfig
    vapidKeys = VAPIDKeys $ ECDSA.KeyPair (ECC.getCurveByName ECC.SEC_p256r1) (vapidKeyPoint) (privateNumber vapidKeysMin)
    vapidKeyPoint = ECC.Point (publicCoordX vapidKeysMin) (publicCoordY vapidKeysMin)
    -- Group subscriptions by host
    subscriptionsMap =
      Map.fromListWith (<>) $ catMaybes ((\sub -> (,[sub]) <$> uriHost (subscriptionEndpoint sub)) <$> subscriptions)

-- |Send a Push Message. Read the message in Service Worker notification handler in browser:
--
-- > self.addEventListener('push', function(event){ console.log(event.data.json()); });
sendPushNotification :: (MonadIO m, A.ToJSON msg, MonadRandom m)
                      => Manager
                      -> VapidConfig
                      -> PushNotification msg
                      -> Subscription
                      -> m (Either PushNotificationError PushNotificationCreated)
sendPushNotification httpManager vapidConfig pushNotification subscription =
  case uriHost (subscriptionEndpoint subscription) of
    Nothing -> pure $ Left $ PushNotificationBadHost (subscriptionEndpoint subscription)
    Just host -> do
      time <- liftIO getPOSIXTime
      let serverIdentification = ServerIdentification {
                serverIdentificationAudience = host
              , serverIdentificationExpiration = round time + fromIntegral (pushNotification ^. pushExpireInSeconds)
              , serverIdentificationSubject = vapidConfigContact vapidConfig
            }
      headers <- hostHeaders vapidKeys serverIdentification
      sendPushNotification' vapidKeys httpManager headers pushNotification subscription
  where
    vapidKeysMin = vapidConfigKey vapidConfig
    vapidKeys = VAPIDKeys $ ECDSA.KeyPair (ECC.getCurveByName ECC.SEC_p256r1) (vapidKeyPoint) (privateNumber vapidKeysMin)
    vapidKeyPoint = ECC.Point (publicCoordX vapidKeysMin) (publicCoordY vapidKeysMin)

-- | Internal function to send a single push notification
sendPushNotification' :: (MonadIO m, A.ToJSON msg, MonadRandom m)
                      => VAPIDKeys
                      -> Manager
                      -> [Header]
                      -> PushNotification msg
                      -> Subscription
                      -> m (Either PushNotificationError PushNotificationCreated)
sendPushNotification' vapidKeys httpManager headers pushNotification subscription = do
  runExceptT $ do
    -- TODO application should check a whitelist of allowed endpoints
    initReq <- withExceptT EndpointParseFailed . ExceptT . liftIO . try . requestFromURI $ subscriptionEndpoint subscription
    ecdhServerPrivateKey <- lift $ ECDH.generatePrivate $ ECC.getCurveByName ECC.SEC_p256r1
    randSalt <- lift $ getRandomBytes 16
    padLen <- lift $ randomRIO (0, 20)

    let encryptionInput = EncryptionInput {
              applicationServerPrivateKey = ecdhServerPrivateKey
            , userAgentPublicKeyBytes = subscriptionPublicKeyBytes
            , authenticationSecret = authSecretBytes
            , salt = randSalt
            , plainText = plainMessage64Encoded
            , paddingLength = padLen
          }
    encryptionOutput <- either (throwError . PushEncryptError) pure $ webPushEncrypt encryptionInput
    let vapidPublicKey = ECDSA.toPublicKey $ unVAPIDKeys vapidKeys
        -- TODO could this be cached
        serverPublic = ECDH.calculatePublic (ECC.getCurveByName ECC.SEC_p256r1) $ ecdhServerPrivateKey
    cryptoKeyHeaderContents <- liftEither $ first ApplicationKeyEncodeError $ cryptoKeyHeader vapidPublicKey serverPublic
    let postHeaders = headers <> [   ("TTL", C8.pack $ show $ pushNotification ^. pushExpireInSeconds)
                        , (hContentType, "application/octet-stream")
                        , ("Crypto-Key", cryptoKeyHeaderContents)
                        , (hContentEncoding, "aesgcm")
                        , ("Encryption", "salt=" <> (B64.URL.encodeBase64Unpadded' randSalt))
                      ]

        request = initReq {
                      method = "POST"
                    , requestHeaders = postHeaders ++
                                            -- avoid duplicate headers
                                            (filter (\(x, _) -> L.notElem x $ map fst postHeaders)
                                                    (requestHeaders initReq)
                                            )
                        -- the body is encrypted message in raw bytes
                        -- without URL encoding
                    , requestBody = RequestBodyBS $ encryptedMessage encryptionOutput
                  }
    resp <- withExceptT onError $ ExceptT $ liftIO $ tryAny $ httpLbs request $ httpManager
    case statusCode (responseStatus resp) of
      201 -> do
        let ttl = parseTTLHeader =<< L.lookup "ttl" (responseHeaders resp)
        pure $ PushNotificationCreated ttl
            
      _ -> throwError $ PushRequestNotCreated resp
  where
    cryptoKeyHeader :: ECDSA.PublicKey -> ECC.Point -> Either String C8.ByteString
    cryptoKeyHeader vapidPublic ecdhServerPublic = do
      let encodePublic = fmap B64.URL.encodeBase64Unpadded' . ecPublicKeyToBytes
      dh <- encodePublic ecdhServerPublic
      ecdsa <- encodePublic (ECDSA.public_q vapidPublic)
      pure $ BS.concat [ "dh=", dh, ";", "p256ecdsa=", ecdsa]
    parseTTLHeader :: BS.ByteString -> Maybe Int
    parseTTLHeader bs = do
      decoded <- either (const Nothing) Just $ TE.decodeUtf8' bs
      either (const Nothing) (Just . fst) $ TR.decimal decoded
    onError :: SomeException -> PushNotificationError
    onError err
      | Just (HttpExceptionRequest _ (StatusCodeException resp _)) <- fromException err = case statusCode (responseStatus resp) of
          -- when the endpoint is invalid, we need to remove it from database
          404 -> RecepientEndpointNotFound
          410 -> RecepientEndpointNotFound
          _   -> PushRequestFailed err
      | otherwise = PushRequestFailed err
    authSecretBytes = B64.URL.decodeBase64Lenient . TE.encodeUtf8 $ subscriptionAuth subscription
    -- extract the 65 bytes of ECDH uncompressed public key received from browser in subscription
    subscriptionPublicKeyBytes = B64.URL.decodeBase64Lenient . TE.encodeUtf8 $ subscriptionP256dh subscription
    -- encode the message to a safe representation like base64URL before sending it to encryption algorithms
    -- decode the message through service workers on browsers before trying to read the JSON
    plainMessage64Encoded = A.encode $ pushNotification ^. pushMessage

type PushP256dh = T.Text
type PushAuth = T.Text

data Subscription = Subscription {
  subscriptionEndpoint :: URI -- ^ Endpoint URI to remote push service
, subscriptionP256dh :: PushP256dh -- ^ Public key of the client
, subscriptionAuth :: PushAuth -- ^ Authentication secret of the client
} deriving (Eq, Ord, Show)

-- | Web push subscription and message details. Use 'mkPushNotification' to construct push notification.
data PushNotification msg = PushNotification {
  _pnExpireInSeconds :: Int64
, _pnMessage :: msg
}

pushExpireInSeconds :: Lens' (PushNotification msg) Int64
pushExpireInSeconds = lens _pnExpireInSeconds (\d v -> d {_pnExpireInSeconds = v})

pushMessage :: (A.ToJSON msg) => Lens (PushNotification a) (PushNotification msg) a msg
pushMessage = lens _pnMessage (\d v -> d {_pnMessage = v})

-- | Constuct a push notification.
--
-- 'PushEndpoint', 'PushP256dh' and 'PushAuth' should be obtained from push subscription in client's browser.
-- Push message can be set through 'pushMessage'; text and json messages are usually supported by browsers.
-- 'pushSenderEmail' and 'pushExpireInSeconds' can be used to set additional details.
mkPushNotification :: PushNotification ()
mkPushNotification =
    PushNotification {
         _pnExpireInSeconds = 3600
       , _pnMessage = ()
    }

-- | 'RecepientEndpointNotFound' comes up when the endpoint is no longer recognized by the push service.
-- This may happen if the user has cancelled the push subscription, and hence deleted the endpoint.
-- You may want to delete the endpoint from database in this case, or if 'EndpointParseFailed'.
data PushNotificationError = EndpointParseFailed HttpException -- ^ Endpoint URL could not be parsed
                           | PushNotificationBadHost URI
                           | PushEncryptError EncryptError
                           | ApplicationKeyEncodeError String -- ^ Application server key encoding failed
                           | RecepientEndpointNotFound -- ^ The endpoint is no longer recognized by the push service
                           | PushRequestFailed SomeException -- ^ Push request failed
                           | PushRequestNotCreated (Response BSL.ByteString) -- ^ Push request failed with non-201 status code
                            deriving (Show, Exception)
