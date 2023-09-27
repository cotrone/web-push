{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.WebPush.Internal where

import           Crypto.Cipher.AES               (AES128)
import qualified Crypto.Cipher.Types             as Cipher
import qualified Crypto.ECC
import           Crypto.Error                    (CryptoError,
                                                  eitherCryptoError)
import           Crypto.Hash.Algorithms          (SHA256 (..))
import qualified Crypto.MAC.HMAC                 as HMAC
import qualified Crypto.PubKey.ECC.DH            as ECDH
import qualified Crypto.PubKey.ECC.ECDSA         as ECDSA
import qualified Crypto.PubKey.ECC.P256          as P256
import qualified Crypto.PubKey.ECC.Types         as ECC
import qualified Crypto.PubKey.ECC.Types         as ECCTypes
import           Crypto.Random
import           Data.Aeson                      ((.=))
import qualified Data.Aeson                      as A
import           Data.Bifunctor
import qualified Data.Binary                     as Binary
import qualified Data.Bits                       as Bits
import qualified Data.ByteArray                  as ByteArray
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LB
import qualified Data.ByteString.Lazy.Base64.URL as B64.URL
import           Data.Data
import           Data.Text                       (Text)
import           Data.Word                       (Word16, Word64, Word8)
import           GHC.Int                         (Int64)
import qualified Data.Text as T
import Network.URI
import Control.Monad.IO.Class
import Network.HTTP.Types

newtype VAPIDKeys = VAPIDKeys {
  unVAPIDKeys :: ECDSA.KeyPair
}

-- | Server identification for a single host, used to identify the server to the remote push server
data ServerIdentification = ServerIdentification {
  serverIdentificationAudience :: Text
, serverIdentificationExpiration :: Int
, serverIdentificationSubject :: Text -- ^ Contact information for the server, either email URI in rfc6068 format 'mailto:text@example.com' or HTTPS URL in rfc2818 format 'https://example.com/contact'
} deriving (Show)
 -- TODO make identification subject a parsed URI

instance A.ToJSON ServerIdentification where
  toJSON p = A.object [
      "aud" .= serverIdentificationAudience p
    , "exp" .= serverIdentificationExpiration p
    , "sub" .= serverIdentificationSubject p
    ]

----------------------------
-- Manual implementation without using the JWT libraries.
-- Not using jose library. Check the below link for reason:
-- https://github.com/sarthakbagaria/web-push/pull/1#issuecomment-471254455
webPushJWT :: MonadRandom m => VAPIDKeys -> ServerIdentification -> m BS.ByteString
webPushJWT vapidKeys payload = do
  -- JWT only accepts SHA256 hash with ECDSA for ES256 signed token
  -- ECDSA signing vulnerable to timing attacks
  signature <- ECDSA.sign (ECDSA.toPrivateKey $ unVAPIDKeys vapidKeys) SHA256 jwtMessage
  pure $ jwtMessage <> "." <> jwtSignature signature
  where
    jwtSignature (ECDSA.Signature signR signS) =
      -- 32 bytes of R followed by 32 bytes of S
      BS.toStrict . B64.URL.encodeBase64Unpadded' $ Binary.encode (int32Bytes signR, int32Bytes signS)
    jwtPayload = B64.URL.encodeBase64Unpadded' . A.encode $ payload
    jwtMessage = BS.toStrict $ jwtHeader <> "." <> jwtPayload
    jwtHeader = B64.URL.encodeBase64Unpadded' . A.encode $ A.object [
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

data EncryptError =
    EncodeApplicationPublicKeyError String
  | EncryptCipherInitError CryptoError
  | EncryptAeadInitError CryptoError
  | EncryptInputPublicKeyError CryptoError
  | EncryptInputApplicationPublicKeyError String
  deriving (Eq, Show)


-- | Payload encryption
-- https://tools.ietf.org/html/draft-ietf-webpush-encryption-04
webPushEncrypt :: WebPushEncryptionInput -> Either EncryptError WebPushEncryptionOutput
webPushEncrypt EncryptionInput{..} = do -- TODO remove record wildcards
  userAgentPublicKey <- first EncryptInputPublicKeyError $ ecBytesToPublicKey userAgentPublicKeyBytes
  applicationServerPublicKey <- first EncryptInputApplicationPublicKeyError . ecPublicKeyToBytes $ ECDH.calculatePublic curveP256 applicationServerPrivateKey
  let
    sharedECDHSecret = ECDH.getShared curveP256 applicationServerPrivateKey userAgentPublicKey
    pseudoRandomKeyCombine = HMAC.hmac authenticationSecret sharedECDHSecret :: HMAC.HMAC SHA256
    inputKeyingMaterial = HMAC.hmac pseudoRandomKeyCombine (authInfo <> "\x01") :: HMAC.HMAC SHA256
    pseudoRandomKeyEncryption = HMAC.hmac salt inputKeyingMaterial :: HMAC.HMAC SHA256
    nonce = BS.pack $ take 12 $ ByteArray.unpack (HMAC.hmac pseudoRandomKeyEncryption (nonceContext <> "\x01") :: HMAC.HMAC SHA256)
    contentEncryptionKey = BS.take 16 $ ByteArray.convert (HMAC.hmac pseudoRandomKeyEncryption (contentEncryptionKeyContext <> "\x01") :: HMAC.HMAC SHA256)
    context = "P-256" <> "\x00" <> "\x00" <> "\x41" <> userAgentPublicKeyBytes <> "\x00" <> "\x41" <> applicationServerPublicKey
    contentEncryptionKeyContext = "Content-Encoding: aesgcm" <> "\x00" <> context
    nonceContext = "Content-Encoding: nonce" <> "\x00" <> context
  -- aes_gcm is aead (authenticated encryption with associated data)
  -- use cek as the encryption key and nonce as the initialization vector
  aesCipher :: AES128 <- first EncryptCipherInitError . eitherCryptoError $ Cipher.cipherInit contentEncryptionKey
  aeadGcmCipher <- first EncryptAeadInitError . eitherCryptoError $ Cipher.aeadInit Cipher.AEAD_GCM aesCipher nonce
  let
      -- tag length 16 bytes (maximum), anything less than 16 bytes may not be secure enough
      -- spec says final encrypted size is 16 bits longer than the padded text
      -- NOTE: the final encrypted message must be sent as raw binary data 
      authTag = ByteArray.convert $ Cipher.unAuthTag authTagBytes
      (authTagBytes, cipherText) = Cipher.aeadSimpleEncrypt aeadGcmCipher BS.empty paddedPlainText 16
      encryptedMessage = cipherText <> authTag
      -- HMAC a doesn't have Show instance needed for test suite
      -- so we extract the bytes and store that in WebPushEncryptionOutput
      inputKeyingMaterialBytes = ByteArray.convert inputKeyingMaterial
      sharedECDHSecretBytes = ByteArray.convert sharedECDHSecret
  pure $ EncryptionOutput {..}
  where
    -- padding length encoded in 2 bytes, followed by
    -- padding length times '0' byte, followed by message
    paddedPlainText = LB.toStrict $
                          (Binary.encode (fromIntegral paddingLength :: Word16)) <>
                          (LB.replicate paddingLength (0 :: Word8)) <>
                          plainText
    authInfo = "Content-Encoding: auth" <> "\x00" :: ByteString
    curveP256 = ECCTypes.getCurveByName ECCTypes.SEC_p256r1

-- | Authorization header for a vapid push notification request
-- this is shared between all push notifications sent to a single push service host
hostHeaders :: (MonadIO m, MonadRandom m)
            => VAPIDKeys
            -> ServerIdentification
            -> m [Header]
hostHeaders vapidKeys serverIdentification = do
  jwt <- webPushJWT vapidKeys serverIdentification
  pure [(hAuthorization, "WebPush " <> jwt)]

-- | The host for URI including scheme and port
uriHost :: URI -> Maybe T.Text
uriHost uri = do
  regName <- uriRegName <$> uriAuthority uri
  pure $ T.pack $ uriScheme uri <> "//" <> regName

-- Conversions among integers and bytes
-- The bytes are in network/big endian order.
 {-
    -- DON'T use DER encoding to extract integer bytes
    -- if a 32 byte number can be written in less bytes with leading zeros,
    -- DER encdoing will be shorter than 32 bytes
    -- and decoding to 4 word64 will fail because of short input
 -}
ecPublicKeyToBytes :: ECC.Point -> Either String ByteString
ecPublicKeyToBytes p = ecPublicKeyToBytes' <$> fromECCPoint p
  where
    fromECCPoint ECC.PointO = Left "Invalid public key infinity point"
    fromECCPoint (ECC.Point x y) = Right (x,y)

ecPublicKeyToBytes' :: (Integer, Integer) -> ByteString
ecPublicKeyToBytes' = Crypto.ECC.encodePoint (Proxy :: Proxy Crypto.ECC.Curve_P256R1) . P256.pointFromIntegers

ecBytesToPublicKey :: ByteString -> Either CryptoError ECC.Point
ecBytesToPublicKey =
  eitherCryptoError . fmap toECCPoint . Crypto.ECC.decodePoint (Proxy :: Proxy Crypto.ECC.Curve_P256R1)
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
