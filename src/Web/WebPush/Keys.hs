{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Web.WebPush.Keys where

import           Web.WebPush.Internal

import           Control.Exception
import qualified Crypto.ECC
import qualified Crypto.Number.Serialize    as Serialize
import qualified Crypto.PubKey.ECC.ECDSA    as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Types    as ECC
import           Crypto.Random              (MonadRandom)
import qualified Data.ASN1.BinaryEncoding   as ASN1
import qualified Data.ASN1.Encoding         as ASN1
import           Data.ASN1.Error
import qualified Data.ASN1.Types            as ASN1
import           Data.Bifunctor
import qualified Data.ByteString            as BS
import           Data.PEM
import           Data.Proxy
import           Data.Word                  (Word8)
import           Data.X509
import           Data.X509.EC
import           Data.X509.File

-- | VAPIDKeys are the public and private keys used to sign the JWT
-- authentication token sent for the push sendPushNotification
-- 
-- The key is an ECDSA key pair with the p256 curve
newtype VAPIDKeys = VAPIDKeys {
  unVAPIDKeys :: ECDSA.KeyPair
} deriving (Show)

-- | Get the public key from the VAPID keys
vapidPublicKey :: VAPIDKeys -> ECDSA.PublicKey
vapidPublicKey = ECDSA.toPublicKey . unVAPIDKeys

-- | Errors from reading the VAPID keys from files
data VAPIDKeysError =
    VAPIDKeysPublicKeyError PublicKeyError -- ^ Error reading the public key
  | VAPIDKeysPrivateKeyError PrivateKeyError -- ^ Error reading the private key
  | VAPIDKeysCurveMismatch -- ^ The public and private keys are not on the same curve
  deriving (Show)

-- | Read the public and private keys from files
readVapidKeys :: FilePath -- ^ Path to the public key file
              -> FilePath -- ^ Path to the private key file
              -> IO (Either VAPIDKeysError VAPIDKeys)
readVapidKeys pubKeyPath privKeyPath = do
  pubKey <- readWebPushPublicKey pubKeyPath
  privKey <- readWebPushPrivateKey privKeyPath
  pure $ do
    pub <- first VAPIDKeysPublicKeyError pubKey
    priv <- first VAPIDKeysPrivateKeyError privKey
    if ECDSA.public_curve pub /= ECDSA.private_curve priv
        then  Left VAPIDKeysCurveMismatch
        else Right $ VAPIDKeys $ toKeyPair pub priv

-- | Convert public and private keys to a key pair
toKeyPair :: ECDSA.PublicKey -> ECDSA.PrivateKey -> ECDSA.KeyPair
toKeyPair pub priv = ECDSA.KeyPair (ECDSA.public_curve pub) (ECDSA.public_q pub) (ECDSA.private_d priv)

-- | Errors from reading the VAPID private key from files
data PrivateKeyError =
    PrivateKeyPEMParseError PEMError -- ^ Error parsing the PEM file
  | PrivateKeyUnknownCurveName -- ^ The curve name is not known
  | PrivateKeyWrongCurve ECC.CurveName -- ^ The curve is not p256
  | PrivateKeyInvalidPEM -- ^ The PEM file is not a single private key
  deriving (Show)

-- | Read the private key from a PEM file
--
-- The private key is an ECDSA private number on the p256 curve
readWebPushPrivateKey :: FilePath -> IO (Either PrivateKeyError ECDSA.PrivateKey)
readWebPushPrivateKey fp = do
  keys <- catch (Right <$> readKeyFile fp) (pure . Left . PrivateKeyPEMParseError)
  pure $ toECDSAPrivateKey =<< findleSingleKey =<< keys
  where
    findleSingleKey [PrivKeyEC key] = Right key
    findleSingleKey _ = Left PrivateKeyInvalidPEM
    toECDSAPrivateKey privKey = do
      curveName <- maybe (Left PrivateKeyUnknownCurveName) Right $ ecPrivKeyCurveName privKey
      case curveName of
        ECC.SEC_p256r1 -> do
          let curve = ECC.getCurveByName curveName
          pure $ ECDSA.PrivateKey curve (privkeyEC_priv privKey)
        other -> Left $ PrivateKeyWrongCurve other

-- | Errors from reading the VAPID public key from files
data PublicKeyError =
    PublicKeyPEMParseError PEMError -- ^ PEM encoding error
  | PublicKeyASN1Error ASN1Error -- ^ ASN1 decoding error
  | PublicKeyFromASN1Error String -- ^ Error converting ASN1 to ECDSA public key
  | PublicKeyUnsupportedKeyType -- ^ The key type is not supported
  | PublicKeyUnknownCurve -- ^ The curve is not known
  | PublicKeyUnserialiseError -- ^ Error unserialising the EC point
  | PublicKeyInvalidPEM -- ^ The PEM file is not a single public key
  deriving (Show)

-- | Read the public key from a PEM file
-- 
-- The public key is an ECDSA public point on the p256 curve
readWebPushPublicKey :: FilePath -> IO (Either PublicKeyError ECDSA.PublicKey)
readWebPushPublicKey fp = do
  contents <- BS.readFile fp
  pubKey <- parsePEMPubKey contents
  pure $ toECDSAPubKey =<< pubKey
  where
    ecPubKey (PubKeyEC pubKey) = Right pubKey
    ecPubKey _ = Left PublicKeyUnsupportedKeyType
    parsePEMPubKey str =
      case pemParseBS str of
        Left err -> fail err
        Right [pem] -> pure $ do
          as <- first PublicKeyASN1Error $ ASN1.decodeASN1' ASN1.DER $ pemContent pem
          (key, _) <- first PublicKeyFromASN1Error $ ASN1.fromASN1 as
          ecPubKey key
        Right _ -> pure $ Left PublicKeyInvalidPEM
    toECDSAPubKey pubKey = do
      curve <- maybe (Left PublicKeyUnknownCurve) Right $ ECC.getCurveByName <$> ecPubKeyCurveName pubKey
      point <- maybe (Left PublicKeyUnserialiseError) Right $ unserializePoint curve $ pubkeyEC_pub pubKey
      pure $ ECDSA.PublicKey curve point

-- | Write the public and private keys to files
-- NOTE: This will overwrite any existing files and it does not
-- store keys in the exact same format as they were read in from
-- if they were created with OpenSSL
writeVAPIDKeys :: FilePath -> FilePath -> VAPIDKeys -> IO ()
writeVAPIDKeys pubKeyPath privKeyPath (VAPIDKeys keyPair) = do
  writeKeyPEM pubKeyPath "PUBLIC KEY" $ toPubKey $ ECDSA.toPublicKey keyPair
  writeKeyPEM privKeyPath "EC PRIVATE KEY" $ toPrivKey $ ECDSA.toPrivateKey keyPair
  where
    writeKeyPEM path name = BS.writeFile path . pemWriteBS . PEM name [] . encodeASN1
    encodeASN1 key = ASN1.encodeASN1' ASN1.DER $ ASN1.toASN1 key []

    toPubKey :: ECDSA.PublicKey -> PubKey
    toPubKey = PubKeyEC . PubKeyEC_Named ECC.SEC_p256r1 . serializePoint . ECDSA.public_q

    toPrivKey :: ECDSA.PrivateKey -> PrivKey
    toPrivKey = PrivKeyEC . PrivKeyEC_Named ECC.SEC_p256r1 . ECDSA.private_d


    serializePoint ::  ECC.Point -> SerializedPoint
    serializePoint ECC.PointO = error "can't serialize EC point at infinity"
    serializePoint (ECC.Point x y) =
      SerializedPoint $ BS.pack [4] <> Serialize.i2ospOf_ bytes x <> Serialize.i2ospOf_ bytes y
      where
        bits  = Crypto.ECC.curveSizeBits (Proxy :: Proxy Crypto.ECC.Curve_P256R1)
        bytes = (bits + 7) `div` 8

-- | Generate a new VAPID key pair, this is an ECDSA key pair on the p256 curve
--
-- Store them securely and use them across multiple push notification requests.
generateVAPIDKeys :: MonadRandom m => m (Either String VAPIDKeys)
generateVAPIDKeys = do
  -- SEC_p256r1 is the NIST P-256
  (pubKey, privKey) <- ECC.generate $ ECC.getCurveByName ECC.SEC_p256r1
  pure $ case ECDSA.public_q pubKey of
    ECC.PointO -> Left "Invalid public key generated, public_q is the point at infinity"
    ECC.Point _ _ -> Right $ VAPIDKeys $ toKeyPair pubKey privKey 

-- | Pass the VAPID public key bytes as `applicationServerKey` when calling subscribe
-- on the `PushManager` object on a registered service worker
--
-- > applicationServerKey = new Uint8Array( #{toJSON vapidPublicKeyBytes} )
vapidPublicKeyBytes :: ECDSA.PublicKey -> Either String [Word8]
vapidPublicKeyBytes key =
  case ECDSA.public_q key of
    ECC.PointO -> Left "Invalid public key generated, public_q is the point at infinity"
    ECC.Point x y -> Right $ BS.unpack $ ecPublicKeyToBytes' (x, y)
