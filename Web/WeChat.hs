module Web.WeChat
  ( module Web.WeChat.Types
  , parseInMessage
  , printOutMessage
  , sha1VerifySignature
  , encodeMsg
  , decodeMsg
  ) where


import           Web.WeChat.Internal
import           Web.WeChat.Types
import           Web.WeChat.XMLParse
import           Web.WeChat.XMLPrint

import           Control.Monad.IO.Class
import qualified Data.ByteArray.Encoding as BA
import           Data.ByteString        (ByteString)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.XML.Light
import           Text.XML.Light.Lexer   (XmlSource)



parseInMessage :: XmlSource s => s -> Maybe (Either InEncryptedMessage InMessage)
parseInMessage s = parseXMLDoc s >>= parseInMessage'

printOutMessage :: OutCallbackMessage -> Text
printOutMessage = T.pack . showElement . eltTag "xml" . printOutMessage'

sha1VerifySignature :: ToByteString a => [a] -> ByteString
sha1VerifySignature = BA.convertToBase BA.Base16 . mkVerifySignature

encodeMsg :: (MonadIO m, ToByteString a) => EncodeMsg a -> m (Either String EncodedMessage)
encodeMsg EncodeMsg{..} =
  case verifyAESkeyLength encodeMsgAESKey of
    Left err -> return $ Left err
    Right verifiedAES -> do
      encrypted <- encryptMsg verifiedAES replyMsg appId
      case encrypted of
        Left err  -> return $ Left err
        Right enc -> do
          let sha1sig   = sha1VerifySignature [token,timeStamp,nonce]
              encodedMessage = "<xml>"
                            <> "<Encrypt><![CDATA[" <> enc <> "]]></Encrypt>"
                            <> "<MsgSignature><![CDATA[" <> sha1sig <> "]]></MsgSignature>"
                            <> "<TimeStamp>" <> timeStamp <> "</TimeStamp>"
                            <> "<Nonce><![CDATA[" <> nonce <> "]]></Nonce>"
                            <> "</xml>"
          return $ Right $ Encoded encodedMessage
 where
  replyMsg  = toByteString encodeMsgReplyMsg
  appId     = toByteString encodeMsgAppId
  token     = toByteString encodeMsgToken
  timeStamp = toByteString encodeMsgTimeStamp
  nonce     = toByteString encodeMsgNonce

decodeMsg :: (MonadIO m, ToByteString a) => DecodeMsg a -> m (Either String DecodedMessage)
decodeMsg DecodeMsg{..} =
  case verifyAESkeyLength decodeMsgAESKey of
    Left err -> return $ Left err
    Right verifiedAES -> do
      let decodedSignature = sha1VerifySignature [decodeMsgToken,decodeMsgNonce,decodeMsgTimeStamp,decodeMsgEncrypt]
      if decodedSignature /= signature
        then return $ Left "ValidateSignatureError: decodeMsg failed to validate signature"
        else do
          return . either Left (Right . Decoded) =<< decryptMsg verifiedAES encrypt appId
 where
  appId     = toByteString decodeMsgAppId
  encrypt   = toByteString decodeMsgEncrypt
  signature = toByteString decodeMsgSignature
