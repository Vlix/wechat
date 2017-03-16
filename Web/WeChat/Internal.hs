module Web.WeChat.Internal
  ( module Web.WeChat.Instances
  , module Web.WeChat.Types
  , verifyAESkeyLength
  , mkVerifySignature
  , encryptMsg
  , decryptMsg
  ) where


import           Prelude                hiding (take,(!!),length,repeat)
import           Web.WeChat.Instances
import           Web.WeChat.Types

import           Control.Monad.IO.Class
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Data.Padding
import           Crypto.Error
import           Crypto.Hash
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as BLD
import           Data.Int               (Int32)
import           Data.List              (sort)
import           Data.List.NonEmpty     hiding (sort)
import qualified Data.Map.Strict        as M
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
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
  either (return . Left) continueEncryption $ makeAESIVfromB64 aeskey
 where
  strLen = toByteString . BLD.toLazyByteString . BLD.int32BE $ (fromIntegral $ BS.length text :: Int32)
  continueEncryption (aes,iv) =
    case cipherInit aes of
      CryptoFailed err -> return $ Left $ "IllegalAesKey: encryptMsg failed to make AES key: " <> show err
      CryptoPassed key -> do
        random16 <- sequence $ take 16 $ repeat $ getRandom alphaNums
        let toEncrypt = padPKCS7 $ toByteString random16 <> strLen <> text <> appid
            ciphertxt = cbcEncrypt key iv toEncrypt
            encodedTxt = B64.encode ciphertxt
        return $ Right encodedTxt

decryptMsg :: Monad m => ByteString -> ByteString -> ByteString -> m (Either String ByteString)
decryptMsg aeskey encrypted appid =
  either (return . Left) continueDecryption $ makeAESIVfromB64 aeskey
 where
  b64DecodedEncrypt = B64.decode encrypted
  continueDecryption (aes,iv) =
    case (cipherInit aes,b64DecodedEncrypt) of
      (CryptoPassed key,Right decodedEncrypt) -> do
        let decrypted = cbcDecrypt key iv decodedEncrypt
        case unpadPKCS7 decrypted of
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
      (CryptoFailed err,_) -> return $ Left $ "IllegalAesKey: encryptMsg failed to make AES key: " <> show err
      (_,Left err) -> return $ Left $ "Base64DecodeError: could not decode encrypted message from base64: " <> show err
   where
    word8ToInt :: [Word8] -> Maybe Int
    word8ToInt (a:b:c:d:[]) = go $ fmap fromIntegral [a,b,c,d]
     where
      go (a':b':c':d':[]) = Just $ (a' * 256 ^ (3 :: Int))
                                 + (b' * 256 ^ (2 :: Int))
                                 + (c' * 256)
                                 + d'
      go _ = Nothing
    word8ToInt _ = Nothing

mkVerifySignature :: ToByteString a => [a] -> Digest SHA1
mkVerifySignature = hash . mconcat . sort . fmap toByteString

makeAESIVfromB64 :: ByteString -> Either String (ByteString,IV AES256)
makeAESIVfromB64 aeskey =
  either (const $ Left decodeB64Error) getIV decodedAES
 where
  decodedAES   = B64.decode (aeskey <> "=")
  getIV validAESkey = maybe (Left encryptError) (Right . (,) validAESkey) $ mkIV validAESkey
  decodeB64Error = "DecodeBase64Error: encryptMsg failed to decode (AESkey + =)"
  encryptError   = "EncryptAESError: encryptMsg failed to makeIV of decoded Base64 decoded AESkey"
  mkIV :: ByteString -> Maybe (IV AES256)
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

returnCodes :: M.Map Int Text
returnCodes = M.fromList $
  [((-1) , "System busy")
  ,(0    , "Request succeeded")
  ,(40001, "Verification failed")
  ,(40002, "Invalid certificate type")
  ,(40003, "Invalid Open ID")
  ,(40004, "Invalid media file type")
  ,(40005, "Invalid file type")
  ,(40006, "Invalid file size")
  ,(40007, "Invalid media file ID")
  ,(40008, "Invalid message type")
  ,(40009, "Invalid image file size")
  ,(40010, "Invalid audio file size")
  ,(40011, "Invalid video file size")
  ,(40012, "Invalid thumbnail file size")
  ,(40013, "Invalid App ID")
  ,(40014, "Invalid access token")
  ,(40015, "Invalid menu type")
  ,(40016, "Invalid button quantity")
  ,(40017, "Invalid button quantity")
  ,(40018, "Invalid button name length")
  ,(40019, "Invalid button KEY length")
  ,(40020, "Invalid button URL length")
  ,(40021, "Invalid menu version")
  ,(40022, "Invalid sub-menu levels")
  ,(40023, "Invalid sub-menu button quantity")
  ,(40024, "Invalid sub-menu button type")
  ,(40025, "Invalid sub-menu button name length")
  ,(40026, "Invalid sub-menu button KEY length")
  ,(40027, "Invalid sub-menu button URL length")
  ,(40028, "Invalid custom menu user")
  ,(40029, "Invalid OAuth code")
  ,(40030, "Invalid refresh token")
  ,(40031, "Invalid openid list")
  ,(40032, "Invalid openid list length")
  ,(40033, "Invalid request characters: The character \"\\uxxxx\" cannot be included.")
  ,(40035, "Invalid parameters")
  ,(40038, "Invalid request format")
  ,(40039, "Invalid URL length")
  ,(40050, "Invalid group ID")
  ,(40051, "Invalid group name")
  ,(41001, "Parameter missing: access token")
  ,(41002, "Parameter missing: appid")
  ,(41003, "Parameter missing: refresh token")
  ,(41004, "Parameter missing: secret")
  ,(41005, "Multimedia file data missing")
  ,(41006, "Parameter missing: media id")
  ,(41007, "Sub-menu data missing")
  ,(41008, "Parameter missing: OAuth code")
  ,(41009, "Parameter missing: openid")
  ,(42001, "access token timed out")
  ,(42002, "refresh token timed out")
  ,(42003, "OAuth code timed out")
  ,(43001, "GET request required")
  ,(43002, "POST request required")
  ,(43003, "HTTPS request required")
  ,(43004, "The other user is not yet a follower")
  ,(43005, "The other user is not yet a follower")
  ,(44001, "Multimedia file is empty")
  ,(44002, "POST package is empty")
  ,(44003, "Rich media message is empty")
  ,(44004, "Text message is empty")
  ,(45001, "Error source: multimedia file size")
  ,(45002, "Message contents too long")
  ,(45003, "Title too long")
  ,(45004, "Description too long")
  ,(45005, "URL too long")
  ,(45006, "Image URL too long")
  ,(45007, "Audio play time over limit")
  ,(45008, "Rich media messages over limit")
  ,(45009, "Error source: interface call")
  ,(45010, "Message quantity over limit")
  ,(45015, "Response too late")
  ,(45016, "System group cannot be changed.")
  ,(45017, "System name too long")
  ,(45018, "Too many groups")
  ,(46001, "Media data missing")
  ,(46002, "This menu version does not exist.")
  ,(46003, "This menu data does not exist.")
  ,(46004, "This user does not exist.")
  ,(47001, "Error while extracting JSON/XML contents")
  ,(48001, "Unauthorized API function")
  ,(48004, "API is banned, please see more information on mp.weixin.qq.com")
  ,(50001, "The user is not authorized for this API")
  ,(61450, "System error (system error)")
  ,(61451, "Invalid parameter (invalid parameter)")
  ,(61452, "Invalid customer service account (invalid kf_account)")
  ,(61453, "Existing customer service account (kf_account existed)")
  ,(61454, "Length of customer service account name over limit (ten English characters at a maximum, excluding @ and the part after it) (invalid kf_acount length)")
  ,(61455, "Invalid characters in a customer service account name (English letters and numbers supported only) (illegal character in kf_account)")
  ,(61456, "Maximum number of customer service accounts reached(ten customer service accounts at a maximum) (kf_account count exceeded)")
  ,(61457, "Invalid image file type (invalid file type)")
  ,(61500, "Date format error")
  ,(61501, "Date range error")
  ,(65301, "This menuid does not match any existed personalized menu")
  ,(65302, "No matching user")
  ,(65303, "No default menu. Cannot create personalized menu")
  ,(65304, "MatchRule cannot be null")
  ,(65305, "Too many personalized menu")
  ,(65306, "Current account does not support personalized menu")
  ,(65307, "Personalized menu cannot be null")
  ,(65308, "All buttons need specified responses")
  ,(65309, "Personalized menu is off")
  ,(65310, "Country cannot be empty")
  ,(65311, "Province cannot be empty")
  ,(65312, "Invalid country")
  ,(65313, "Invalid province")
  ,(65314, "Invalid city")
  ,(65316, "Current menu has too many sub-domains")
  ,(65317, "Invalid URL")
  ,(9001001, "Invalid POST request")
  ,(9001002, "Remote service is not available")
  ,(9001003, "Invalid Ticket")
  ,(9001004, "Fail to load people around you")
  ,(9001005, "Fail to load merchant")
  ,(9001006, "Fail to load OpenID")
  ,(9001007, "Missing upload files")
  ,(9001008, "Invalid upload file type")
  ,(9001009, "Invalid upload file size")
  ,(9001010, "Fail to upload file")
  ,(9001020, "Invalid account")
  ,(9001021, "Active device rate below 50%, cannot add new device")
  ,(9001022, "Invalid number of device (Need at least one device)")
  ,(9001023, "Device ID is under review")
  ,(9001024, "Too many search items (50 device ID at most)")
  ,(9001025, "Invalid device ID")
  ,(9001026, "Invalid screen ID")
  ,(9001027, "Invalid page parameters")
  ,(9001028, "Can only delete 10 screen IDs at a time")
  ,(9001029, "Screen exists, please remove relation before deletion")
  ,(9001030, "Can only search 50 screen IDs at a time")
  ,(9001031, "Invalid time range")
  ,(9001032, "Invalid binding of saved device and screen")
  ,(9001033, "Invalid retail listing ID")
  ,(9001034, "Device remark is too long")
  ,(9001035, "Invalid application parameters")
  ,(9001036, "Invalid search of begin value")
  ]