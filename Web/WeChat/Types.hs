module Web.WeChat.Types where


import           Data.ByteString        (ByteString)


newtype EncodedMessage = Encoded { encMsg :: ByteString }

data EncodeMsg a = EncodeMsg
  { encodeMsgReplyMsg  :: a -- XML message to be sent to WeChat
  , encodeMsgToken     :: a
  , encodeMsgAESKey    :: a
  , encodeMsgAppId     :: a
  , encodeMsgTimeStamp :: a
  , encodeMsgNonce     :: a
  }


newtype DecodedMessage = Decoded { decMsg :: ByteString }

data DecodeMsg a = DecodeMsg
  { decodeMsgEncrypt   :: a -- XML message to be sent to WeChat
  , decodeMsgSignature :: a
  , decodeMsgToken     :: a
  , decodeMsgAESKey    :: a
  , decodeMsgAppId     :: a
  , decodeMsgTimeStamp :: a
  , decodeMsgNonce     :: a
  }
