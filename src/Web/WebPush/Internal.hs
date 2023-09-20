{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.WebPush.Internal where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Crypto.Cipher.AES          (AES128)
import qualified Crypto.Cipher.Types        as Cipher
import qualified Crypto.ECC
import           Crypto.Error               (CryptoError, eitherCryptoError)
import           Crypto.Hash.Algorithms     (SHA256 (..))
import qualified Crypto.MAC.HMAC            as HMAC
import qualified Crypto.PubKey.ECC.DH       as ECDH
import qualified Crypto.PubKey.ECC.ECDSA    as ECDSA
import qualified Crypto.PubKey.ECC.P256     as P256
import qualified Crypto.PubKey.ECC.Types    as ECC
import qualified Crypto.PubKey.ECC.Types    as ECCTypes
import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as A
import           Data.Bifunctor
import qualified Data.Binary                as Binary
import qualified Data.Bits                  as Bits
import qualified Data.ByteArray             as ByteArray
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64.URL as B64.URL
import qualified Data.ByteString.Lazy       as LB
import           Data.Data
import           Data.Text                  (Text)
import           Data.Word                  (Word16, Word64, Word8)
import           GHC.Int                    (Int64)

type VAPIDKeys = ECDSA.KeyPair

data PushNotificationPayload = PushNotificationPayload {
  payloadAudience :: Text
, payloadExpiration :: Int
, payloadSubject :: Text
} deriving (Show)

instance A.ToJSON PushNotificationPayload where
  toJSON p = A.object [
      "aud" .= payloadAudience p
    , "exp" .= payloadExpiration p
    , "sub" .= payloadSubject p
    ]

----------------------------
-- Manual implementation without using the JWT libraries.
-- Not using jose library. Check the below link for reason:
-- https://github.com/sarthakbagaria/web-push/pull/1#issuecomment-471254455
webPushJWT :: MonadIO m => VAPIDKeys -> PushNotificationPayload -> m LB.ByteString
webPushJWT vapidKeys payload = do
  let
    encodedJWTPayload = B64.URL.encodeBase64Unpadded' . LB.toStrict . A.encode $ payload
    messageForJWTSignature = encodedJWTHeader <> "." <> encodedJWTPayload
  -- JWT only accepts SHA256 hash with ECDSA for ES256 signed token
  -- ECDSA signing vulnerable to timing attacks
  ECDSA.Signature signR signS <- liftIO $ ECDSA.sign (ECDSA.toPrivateKey vapidKeys) SHA256 messageForJWTSignature
  -- 32 bytes of R followed by 32 bytes of S
  let encodedJWTSignature = B64.URL.encodeBase64Unpadded' $ LB.toStrict $ (Binary.encode $ int32Bytes signR) <> (Binary.encode $ int32Bytes signS)
  pure . LB.fromStrict $ messageForJWTSignature <> "." <> encodedJWTSignature
  where
    encodedJWTHeader = B64.URL.encodeBase64Unpadded' . LB.toStrict . A.encode $ A.object [
        "typ" .= ("JWT" :: Text)
      , "alg" .= ("ES256" :: Text)
      ]

-- | All inputs are in raw bytes with no encoding
-- except for the plaintext for which raw bytes are the Base 64 encoded bytes
data WebPushEncryptionInput = EncryptionInput {
  applicationServerPrivateKey :: ECDH.PrivateNumber
, userAgentPublicKeyBytes :: ByteString
, authenticationSecret :: ByteString
, salt :: ByteString
, plainText :: LB.ByteString
, paddingLength :: Int64
} deriving (Show)

-- | Intermediate encryption output used in tests
-- All in raw bytes
data WebPushEncryptionOutput = EncryptionOutput {
  sharedECDHSecretBytes :: ByteString
, inputKeyingMaterialBytes :: ByteString
, contentEncryptionKeyContext :: ByteString
, contentEncryptionKey :: ByteString
, nonceContext :: ByteString
, nonce :: ByteString
, paddedPlainText :: ByteString
, encryptedMessage :: ByteString
}

data PushEncryptError =
    PushEncryptCryptoError CryptoError
  | PushEncryptParseKeyError CryptoError
  | PushEncodeApplicationPublicKeyError String
  deriving (Eq, Show)

-- | payload encryption
-- https://tools.ietf.org/html/draft-ietf-webpush-encryption-04
webPushEncrypt :: WebPushEncryptionInput -> Either PushEncryptError WebPushEncryptionOutput
webPushEncrypt EncryptionInput {..} = do
  userAgentPublicKey <- first PushEncryptParseKeyError $ ecBytesToPublicKey userAgentPublicKeyBytes
  applicationServerPublicKeyBytes <- first PushEncodeApplicationPublicKeyError $ ecPublicKeyToBytes $ ECDH.calculatePublic curveP256 applicationServerPrivateKey
  let 
    sharedECDHSecret = ECDH.getShared curveP256 applicationServerPrivateKey userAgentPublicKey

    -- HMAC key derivation (HKDF, here expanded into HMAC steps as specified in web push encryption spec)
    pseudoRandomKeyCombine = HMAC.hmac authenticationSecret sharedECDHSecret :: HMAC.HMAC SHA256
    authInfo = "Content-Encoding: auth" <> "\x00" :: ByteString
    inputKeyingMaterial = HMAC.hmac pseudoRandomKeyCombine (authInfo <> "\x01") :: HMAC.HMAC SHA256

    context = "P-256" <> "\x00" <> "\x00" <> "\x41" <> userAgentPublicKeyBytes <> "\x00" <> "\x41" <> applicationServerPublicKeyBytes

    pseudoRandomKeyEncryption = HMAC.hmac salt inputKeyingMaterial :: HMAC.HMAC SHA256
    contentEncryptionKeyContext = "Content-Encoding: aesgcm" <> "\x00" <> context
    contentEncryptionKey = BS.pack $ take 16 $ ByteArray.unpack (HMAC.hmac pseudoRandomKeyEncryption (contentEncryptionKeyContext <> "\x01") :: HMAC.HMAC SHA256)

    nonceContext = "Content-Encoding: nonce" <> "\x00" <> context
    nonce = BS.pack $ take 12 $ ByteArray.unpack (HMAC.hmac pseudoRandomKeyEncryption (nonceContext <> "\x01") :: HMAC.HMAC SHA256)

    -- HMAC a doesn't have Show instance needed for test suite
    -- so we extract the bytes and store that in WebPushEncryptionOutput
    inputKeyingMaterialBytes = ByteArray.convert inputKeyingMaterial
    sharedECDHSecretBytes = ByteArray.convert sharedECDHSecret

    -- padding length encoded in 2 bytes, followed by
    -- padding length times '0' byte, followed by
    -- message
    paddedPlainText = LB.toStrict $
                          (Binary.encode (fromIntegral paddingLength :: Word16)) <>
                          (LB.replicate paddingLength (0 :: Word8)) <>
                          plainText

  -- aes_gcm is aead (authenticated encryption with associated data)
  -- use cek as the encryption key and nonce as the initialization vector
  aesCipher :: AES128 <- handleCryptoError $ Cipher.cipherInit contentEncryptionKey
  aeadGcmCipher <- handleCryptoError $ Cipher.aeadInit Cipher.AEAD_GCM aesCipher nonce
  -- tag length 16 bytes (maximum), anything less than 16 bytes may not be secure enough
  -- spec says final encrypted size is 16 bits longer than the padded text
  -- NOTE: the final encrypted message must be sent as raw binary data
  let
    (authTagBytes, cipherText) = Cipher.aeadSimpleEncrypt aeadGcmCipher BS.empty paddedPlainText 16
    authTag = ByteArray.convert $ Cipher.unAuthTag authTagBytes
    encryptedMessage = cipherText <> authTag
  pure $ EncryptionOutput {..}
  where
    handleCryptoError = first PushEncryptCryptoError . eitherCryptoError
    curveP256 = ECCTypes.getCurveByName ECCTypes.SEC_p256r1

-- Conversions among integers and bytes
-- The bytes are in network/big endian order.
 {-
    -- DON'T use DER encoding to extract integer bytes
    -- if a 32 byte number can be written in less bytes with leading zeros,
    -- DER encdoing will be shorter than 32 bytes
    -- and decoding to 4 word64 will fail because of short input
 -}
ecPublicKeyToBytes :: ECC.Point -> Either String ByteString
ecPublicKeyToBytes p = Crypto.ECC.encodePoint (Proxy :: Proxy Crypto.ECC.Curve_P256R1) <$> fromECCPoint p
  where
    fromECCPoint ECC.PointO = Left "Invalid public key infinity point"
    fromECCPoint (ECC.Point x y) = Right $ P256.pointFromIntegers (x,y)

ecBytesToPublicKey :: ByteString -> Either CryptoError ECC.Point
ecBytesToPublicKey = eitherCryptoError . fmap toECCPoint . Crypto.ECC.decodePoint (Proxy :: Proxy Crypto.ECC.Curve_P256R1)
  where toECCPoint = uncurry ECC.Point . P256.pointToIntegers 

-- Coordinates on Elliptic Curves are 32 bit integers
type Bytes32 = (Word64, Word64, Word64, Word64)

 -- points on elliptic curve for 256 bit algorithms are 32 bytes (256 bits) unsigned integers each
 -- fixed width big endian format
 -- integer to 4 Word64 (8 bytes each)
int32Bytes :: Integer -> Bytes32
int32Bytes number =  let shift1 = Bits.shiftR number 64
                         shift2 = Bits.shiftR shift1 64
                         shift3 = Bits.shiftR shift2 64
                     in ( fromIntegral shift3
                        , fromIntegral shift2
                        , fromIntegral shift1
                        , fromIntegral number
                       )

bytes32Int :: Bytes32 -> Integer
bytes32Int (d,c,b,a) = (Bits.shiftL (fromIntegral d) (64*3)) +
                       (Bits.shiftL (fromIntegral c) (64*2)) +
                       (Bits.shiftL (fromIntegral b) (64  )) +
                                    (fromIntegral a)
