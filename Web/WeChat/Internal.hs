module Web.WeChat.Internal
  ( module Web.WeChat.Instances
  , module Web.WeChat.Types
  , EncodeMsg (..)
  , EncodedMessage (..)
  , verifyAESkeyLength
  , sha1VerifySignature
  , encryptMsg
  , decryptMsg
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
import           Data.Word
import           System.Random


verifyAESkeyLength :: ToByteString a => a -> Either String ByteString
verifyAESkeyLength x =
  case BS.length aesKey of
    43 -> Right aesKey
    _  -> Left "IllegalAesKey: length other than 43"
 where
  aesKey = toByteString x

encryptMsg :: MonadIO m => ByteString -> ByteString -> ByteString -> m (Either String ByteString)
encryptMsg aeskey text appid =
  either (return . Left) continueEncryption $ makeIVfromB64 aeskey
 where
  strLen = toByteString . BLD.toLazyByteString . BLD.int32BE $ (fromIntegral $ BS.length text :: Int32)
  continueEncryption iv = do
    random16 <- sequence $ take 16 $ repeat $ getRandom alphaNums
    let toEncrypt' = toByteString random16 <> strLen <> text <> appid
        toEncrypt  = padPKCS7 toEncrypt'
        ciphertxt  = cbcEncrypt (undefined :: AES128) iv toEncrypt
    return $ Right ciphertxt

decryptMsg :: Monad m => ByteString -> ByteString -> ByteString -> m (Either String ByteString)
decryptMsg aeskey encrypted appid =
  either (return . Left) continueDecryption $ makeIVfromB64 aeskey
 where
  continueDecryption iv =
    case mUnpadded of
      Nothing -> return $ Left "PKCS7ParseError: decryptMsg failed to unpad the decrypted message"
      Just unpadded -> do
        let removedRandom16 = BS.drop 16 unpadded
            (lengthBS,rest) = BS.splitAt 4 removedRandom16
            mMsgLength = word8ToInt $ BS.unpack lengthBS
        case mMsgLength of
          Nothing -> return $ Left "ContentLengthError: decryptMsg failed to get 4-length byte sequence"
          Just msgLength -> do
            let (content,appIdToVerify) = BS.splitAt msgLength rest
            if appIdToVerify /= appid
              then return $ Left "ValidateAppidError: decryptMsg failed to verify validity of provided AppID"
              else return $ Right content
   where
    decrypted = cbcDecrypt (undefined :: AES128) iv encrypted
    mUnpadded = unpadPKCS7 decrypted
    word8ToInt :: [Word8] -> Maybe Int
    word8ToInt (a:b:c:d:[]) = Just $ (a' * 16 ^ 3) + (b' * 16 ^ 2) + (c' * 16) + d'
     where
      a' = fromIntegral a
      b' = fromIntegral b
      c' = fromIntegral c
      d' = fromIntegral d
    word8ToInt _ = Nothing


sha1VerifySignature :: ToByteString a => [a] -> ByteString
sha1VerifySignature = BS.pack . BA.unpack . mkVerifySignature

mkVerifySignature :: ToByteString a => [a] -> Digest SHA1
mkVerifySignature = hash . mconcat . sort . fmap toByteString


makeIVfromB64 :: ByteString -> Either String (IV AES128)
makeIVfromB64 aeskey =
  either (const $ Left decodeB64Error) getIV $ B64.decode (aeskey <> "=")
 where
  getIV          = maybe (Left encryptError) Right . mkIV
  decodeB64Error = "DecodeBase64Error: encryptMsg failed to decode (AESkey + =)"
  encryptError   = "EncryptAESError: encryptMsg failed to makeIV of decoded Base64 decoded AESkey"
  mkIV :: ByteString -> Maybe (IV AES128)
  mkIV = makeIV . BS.take 16

padPKCS7 :: ByteString -> ByteString
padPKCS7 = pad $ PKCS7 32

unpadPKCS7 :: ByteString -> Maybe ByteString
unpadPKCS7 = unpad $ PKCS7 32

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
