{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Web.WebPush (
-- * Functions
  sendPushNotification
, sendPushNotifications
-- * Types
, Subscription(..)
, VAPIDConfig(..)
, PushNotification(..)
, PushNotificationCreated(..)
, PushNotificationError(..)
, PushP256dh
, PushAuth
, module Web.WebPush.Keys
) where

import           Web.WebPush.Internal
import           Web.WebPush.Keys

import           Control.Exception
import           Control.Exception.Safe     (tryAny)
import           Control.Monad.Except
import qualified Crypto.PubKey.ECC.DH       as ECDH
import qualified Crypto.PubKey.ECC.ECDSA    as ECDSA
import qualified Crypto.PubKey.ECC.Types    as ECC
import           Crypto.Random              (MonadRandom (getRandomBytes))
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Encoding        as AE
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
import qualified Network.URI                as URI
import           System.Random              (randomRIO)
import           Web.FormUrlEncoded
import           Web.HttpApiData

-- | Configuration for VAPID server identification
data VAPIDConfig = VAPIDConfig {
  vapidConfigContact :: T.Text -- ^ Contact information for the application server, either a `mailto:` URI or an HTTPS URL
, vapidConfigKeys :: VAPIDKeys -- ^ Keypair used to sign the VAPID identification
}

-- | Result of a successful push notification request
data PushNotificationCreated = PushNotificationCreated {
  pushNotificationCreatedTTL :: Maybe Int -- ^ Optional TTL of the notification
} deriving (Eq, Ord, Show)

-- |Send a push notification to multiple subscribers
-- similar to `sendPushNotification` but shares VAPID keys across multiple requests
sendPushNotifications :: (MonadIO m, A.ToJSON msg, MonadRandom m)
                      => Manager
                      -> VAPIDConfig
                      -> PushNotification msg
                      -> [Subscription]
                      -> m [(Subscription, Either PushNotificationError PushNotificationCreated)]
sendPushNotifications httpManager vapidConfig pushNotification subscriptions = do
  fmap concat $ forM (Map.toList subscriptionsMap) $ \(host, hostSubscriptions) -> do
    time <- liftIO getPOSIXTime
    let serverIdentification = ServerIdentification {
              serverIdentificationAudience = host
            , serverIdentificationExpiration = round time + fromIntegral (pnExpireInSeconds pushNotification)
            , serverIdentificationSubject = vapidConfigContact vapidConfig
          }
    headers <- hostHeaders privateKey serverIdentification
    forM hostSubscriptions $ \subscription -> do
      e <- sendPushNotification' vapidKeys httpManager headers pushNotification subscription
      pure (subscription, e)
  where
    privateKey = ECDSA.toPrivateKey $ unVAPIDKeys vapidKeys
    vapidKeys = vapidConfigKeys vapidConfig
    -- Group subscriptions by host
    subscriptionsMap =
      Map.fromListWith (<>) $ catMaybes ((\sub -> (,[sub]) <$> uriHost (subscriptionEndpoint sub)) <$> subscriptions)

-- | Send a Push Message. Read the message in Service Worker notification handler in browser:
--
-- > self.addEventListener('push', function(event){ console.log(event.data.json()); });
sendPushNotification :: (MonadIO m, A.ToJSON msg, MonadRandom m)
                      => Manager
                      -> VAPIDConfig
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
              , serverIdentificationExpiration = round time + fromIntegral (pnExpireInSeconds pushNotification)
              , serverIdentificationSubject = vapidConfigContact vapidConfig
            }
      headers <- hostHeaders privateKey serverIdentification
      sendPushNotification' vapidKeys httpManager headers pushNotification subscription
  where
    privateKey = ECDSA.toPrivateKey $ unVAPIDKeys vapidKeys
    vapidKeys = vapidConfigKeys vapidConfig

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
    -- TODO could this be cached
    let serverPublic = ECDH.calculatePublic (ECC.getCurveByName ECC.SEC_p256r1) $ ecdhServerPrivateKey
    cryptoKeyHeaderContents <- liftEither $ first ApplicationKeyEncodeError $ cryptoKeyHeader (vapidPublicKey vapidKeys) serverPublic
    let postHeaders = headers <> [   ("TTL", C8.pack $ show $ pnExpireInSeconds pushNotification)
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
    plainMessage64Encoded = A.encode $ pnMessage pushNotification

type PushP256dh = T.Text
type PushAuth = T.Text

-- | Subscription information for a push notification
data Subscription = Subscription {
  subscriptionEndpoint :: URI -- ^ Endpoint URI to remote push service
, subscriptionP256dh :: PushP256dh -- ^ Public key of the client
, subscriptionAuth :: PushAuth -- ^ Authentication secret of the client
} deriving (Eq, Ord, Show)

instance A.FromJSON Subscription where
  parseJSON = A.withObject "Subscription" $ \obj -> do
    uri <- unWPURI <$> obj A..: "endpoint"
    p256 <- obj A..: "p256dh"
    auth' <- obj A..: "auth"
    pure $ Subscription {
        subscriptionEndpoint = uri
      , subscriptionP256dh = p256
      , subscriptionAuth = auth'
      }

instance A.ToJSON Subscription where
  toJSON sub = A.object [
      "endpoint" A..= WPURI (subscriptionEndpoint sub)
    , "p256dh" A..= subscriptionP256dh sub
    , "auth" A..= subscriptionAuth sub
    ]

-- | Wrapper around URI to parse and serialize URI in JSON and URL encoded forms
-- Aeson added a URI instance in 2.2.0.0 but webdriver-w3c doesn't compile with that version
-- see https://github.com/nbloomf/webdriver-w3c/pull/64
newtype WPURI = WPURI {
  unWPURI :: URI
} deriving (Eq, Ord, Show)

instance FromHttpApiData WPURI where
  parseUrlPiece = maybe (Left "Invalid URI") (Right . WPURI) . parseURI . T.unpack

instance A.FromJSON WPURI where
  parseJSON = A.withText "URI" $ \t ->
    case URI.parseURI (T.unpack t) of
      Nothing -> fail "Invalid URI"
      Just x  -> pure $ WPURI x

instance A.ToJSON WPURI where
    toJSON uri = A.toJSON (URI.uriToString id (unWPURI uri) "")
    toEncoding uri = AE.string (URI.uriToString id (unWPURI uri) "")

instance FromForm Subscription where
  fromForm form = do
    uri <- unWPURI <$> parseUnique "endpoint" form
    p256 <- parseUnique "p256dh" form 
    auth' <- parseUnique "auth" form
    pure $ Subscription {
        subscriptionEndpoint = uri
      , subscriptionP256dh = p256
      , subscriptionAuth = auth'
      }

-- | Web push notification expiration and message to send
data PushNotification msg = PushNotification {
  pnExpireInSeconds :: Int -- ^ Expiration time in seconds
, pnMessage :: msg -- ^ Message to send
} deriving (Eq, Ord, Show)

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
