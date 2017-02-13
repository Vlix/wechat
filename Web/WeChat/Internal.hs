module Web.WeChat.Internal
  ( module Web.WeChat.Instances
  , module Web.WeChat.Types
  , EncodeMsg (..)
  , EncodedMessage (..)
  , sha1VerifySignature
  , encryptMsg
  ) where


import           Prelude                hiding (take,(!!),length,repeat)
import           Web.WeChat.Instances
import           Web.WeChat.Types

import           Control.Monad          (sequence)
import           Control.Monad.IO.Class
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Data.Padding
import           Crypto.Hash
import qualified Data.ByteArray         as BA
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as BLD
import           Data.Int               (Int32)
import           Data.List              (sort)
import           Data.List.NonEmpty     hiding (sort)
import           Data.Semigroup         ((<>))
import           System.Random


padPKCS7 :: ByteString -> ByteString
padPKCS7 = pad $ PKCS7 32

unpadPKCS7 :: ByteString -> Maybe ByteString
unpadPKCS7 = unpad $ PKCS7 32

-- NOT SURE IF BS.length ...
encryptMsg :: MonadIO m => ByteString -> ByteString -> ByteString -> m (Either String ByteString)
encryptMsg aeskey text appid =
  case B64.decode (aeskey <> "=") of
    Left err  -> return $ Left "DecodeBase64Error: encryptMsg failed to decode (AESkey + =)"
    Right b64 ->
      case mkIV b64 of
        Nothing -> return $ Left "EncryptAESError: encryptMsg failed to makeIV of decoded Base64 decoded AESkey"
        Just iv -> do
          random16 <- sequence $ take 16 $ repeat $ getRandom alphaNums
          let toEncrypt' = toByteString random16 <> strLen <> text <> appid
              toEncrypt  = padPKCS7 toEncrypt'
              ciphertxt  = cbcEncrypt (undefined :: AES128) iv toEncrypt
          return $ Right ciphertxt
 where
  strLen = toByteString . BLD.toLazyByteString . BLD.int32BE $ (fromIntegral $ BS.length text :: Int32)
  mkIV :: ByteString -> Maybe (IV AES128)
  mkIV = makeIV . BS.take 16

--decryptMsg :: 

sha1VerifySignature :: ToByteString a => [a] -> ByteString
sha1VerifySignature = BS.pack . BA.unpack . mkVerifySignature

mkVerifySignature :: ToByteString a => [a] -> Digest SHA1
mkVerifySignature = hash . mconcat . sort . fmap toByteString

verifyAESkeyLength :: ToByteString a => a -> Either String ByteString
verifyAESkeyLength x =
  case BS.length aesKey of
    43 -> Right aesKey
    _  -> Left "IllegalAesKey: length other than 43"
 where
  aesKey = toByteString x

alphaNums :: NonEmpty Char
alphaNums = numbers <> upperC <> lowerC
 where
  numbers = '0' :| ['1'..'9']
  upperC  = 'A' :| ['B'..'Z']
  lowerC  = 'a' :| ['b'..'z']

getRandom :: MonadIO m => NonEmpty a -> m a
getRandom (x :| []) = return x
getRandom list = do
    r <- liftIO $ (randomIO :: IO Int)
    let x1 = r `mod` (length list)
    return $ list !! x1
