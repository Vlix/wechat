module Web.WeChat.Types where


import           Data.ByteString        (ByteString)
import           Data.Text              (Text)


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
  { decodeMsgEncrypt   :: a -- encrypted XML message sent from WeChat
  , decodeMsgSignature :: a
  , decodeMsgToken     :: a
  , decodeMsgAESKey    :: a
  , decodeMsgAppId     :: a
  , decodeMsgTimeStamp :: a
  , decodeMsgNonce     :: a
  }


--------------
-- INCOMING --
--------------

type MediaID = Text
type URL = Text

data InMessageContent
   = InText { inTextContent :: Text }
   | InImage { inPicURL :: URL, inMediaID :: MediaID }
   | InAudio { inMediaID :: MediaID, inFormat :: Text }
   | InVideo { inMediaID :: MediaID, inThumbMediaID :: MediaID }
   | InLocation { inLat :: Double, inLon :: Double, inScale :: Int, inLabel :: Text}
   | InLink { inTitle :: Text, inDescription :: Text, inUrl :: URL }
   | InEvent {inEvent :: InEvent }
   | InSpeechRecognition { inMediaID :: MediaID, inFormat :: Text, inRecognition :: Text }
  deriving (Eq, Show)

data InEvent
  = Subscribe
  | Unsubscribe
  | QRSubscribe { eventKey :: Text, eventTicket :: Text }
  | QRScan { eventKey :: Text, eventTicket :: Text }
  | Location { eventLat :: Double, eventLon :: Double, eventPrecision :: Double }
  | Click { eventKey :: Text }
  | Redirect { eventKey :: Text }
  deriving (Eq, Show, Read)

data InMessage = InMessage
  { inFrom       :: Text
  , inTo         :: Text
  , inCreateTime :: Integer
  , inContent    :: InMessageContent
  , inMsgId      :: Maybe Integer
  } deriving (Eq, Show)

data InEncryptedMessage = InEncryptedMessage
  { inEncrypt      :: Text
  , inMsgSignature :: Text
  , inTimeStamp    :: Integer
  , inNonce        :: Text
  } deriving (Eq, Show)


--------------
-- OUTGOING --
--------------

data OutMessageContent
   = OutText  { outTextContent :: Text }
   | OutImage { outMediaID :: MediaID }
   | OutAudio { outMediaID :: MediaID }
   | OutVideo { outMediaID :: MediaID, outThumbMediaID :: MediaID }
   | OutMusic { outTitle :: Maybe Text, outDescription :: Maybe Text, outMusicURL :: Maybe URL, outHQMusicURL :: Maybe URL, outThumbMediaID :: MediaID }
   | OutRich  { outArticles :: [OutRichArticle] }

data OutRichArticle = OutRichArticle { outArticleTitle       :: Maybe Text
                                     , outArticleDescription :: Maybe Text
                                     , outPicURL             :: Maybe URL
                                     , outURL                :: Maybe URL
                                     }

data OutCallbackMessage = OutMessage { outCbFrom       :: Text
                                     , outCbTo         :: Text
                                     , outCbCreateTime :: Integer
                                     , outCbContent    :: OutMessageContent
                                     }

data OutCSMessage = OutCSMessage { outCSTo          :: Text
                                 , outCSContent     :: OutMessageContent
                                 }


---------------
-- RESPONSES --
---------------

data ErrorResponse = ErrorResponse { errorCode :: Int
                                   , errorMsg  :: Text
                                   , msgId     :: Maybe Int
                                   }

data MultimediaTransferResponse = MultimediaTransferResponse
  { multimediaType    :: Text
  , multimediaId      :: Text
  , multimediaCreated :: Int
  }

data AccessTokenResponse = AccessTokenResponse { accessToken :: Text, accessTokenExpires :: Int }
