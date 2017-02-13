module Web.WeChat where


import           Web.WeChat.Internal

import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import           Data.Monoid            ((<>))


encodeMsg :: (MonadIO m, ToByteString a) => EncodeMsg a -> m (Either String EncodedMessage)
encodeMsg EncodeMsg{..} =
  case verifyAESkeyLength encodeMsgAESkey of
    Left err -> return $ Left err
    Right verifiedAES -> do
      let replyMsg  = toByteString encodeMsgReplyMsg
          appId     = toByteString encodeMsgAppId
      encrypted <- encryptMsg replyMsg appId verifiedAES
      case encrypted of
        Left err  -> return $ Left err
        Right enc -> do
          let token     = toByteString encodeMsgToken
              timeStamp = toByteString encodeMsgTimeStamp
              nonce     = toByteString encodeMsgNonce
              sha1sig   = sha1VerifySignature [token,timeStamp,nonce]
              encodedMessage = "<xml><Encrypt><![CDATA[" <> enc
                            <> "]]></Encrypt><MsgSignature><![CDATA[" <> sha1sig
                            <> "]]></MsgSignature><TimeStamp>" <> timeStamp
                            <> "</TimeStamp><Nonce><![CDATA[" <> nonce
                            <> "]]></Nonce></xml>"
          return $ Right $ Encoded encodedMessage

decodeMsg :: (MonadIO m, ToByteString a) => DecodeMsg a -> m (Either String DecodedMessage)
decodeMsg DecodeMsg{..} =
  case verifyAESkeyLength aesKey