module Web.WeChat.Types where


import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import           Data.Time              (UTCTime)


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

type MediaID = Text

type URL = Text

data InMessageContent
   = InText { inTextContent :: Text }
   | InImage { inPicURL :: URL, inMediaID :: MediaID }
   | InAudio { inMediaID :: MediaID, inFormat :: Text }
   | InVideo { inMediaID :: MediaID, inThumbMediaID :: MediaID }
   | InLocation { inLat :: Double, inLon :: Double, inScale :: Int, inLabel :: Text}
   | InLink { inTitle :: Text, inDescription :: Text, inUrl :: URL }
   | InEventSubscribe
   | InEventUnsubscribe
   | InEventQRSubscribe { inEventKey :: Text, inTicket :: Text }
   | InEventScan { inEventKey :: Text, inTicket :: Text }
   | InEventLocation { inLat :: Double, inLon :: Double, inPrecision :: Double }
   | InEventClick { inEventKey :: Text }
   | InEventRedirect { inEventKey :: Text }
   | InSpeechRecognition { inMediaID :: MediaID, inFormat :: Text, inRecognition :: Text }
  deriving (Eq, Show)

data InMessage = InMessage { inFrom       :: Text
                           , inTo         :: Text
                           , inCreateTime :: UTCTime
                           , inContent    :: InMessageContent
                           , inMsgId      :: Maybe Integer
                           }
  deriving (Eq, Show)

data OutMessageContent
   = OutText { outTextContent :: Text }
   | OutImage { outMediaID :: MediaID }
   | OutAudio { outMediaID :: MediaID }
   | OutVideo { outMediaID :: MediaID, outThumbMediaID :: MediaID }
   | OutMusic { outTitle :: Maybe Text, outDescription :: Maybe Text, outMusicURL :: Maybe URL, outHQMusicURL :: Maybe URL, outThumbMediaID :: MediaID }
   | OutRich { outArticles :: [OutRichArticle] }

data OutRichArticle = OutRichArticle { outArticleTitle :: Maybe Text
                                     , outArticleDescription :: Maybe Text
                                     , outPicURL :: Maybe URL
                                     , outURL    :: Maybe URL
                                     }

data OutCallbackMessage = OutMessage { outCbFrom       :: Text
                                     , outCbTo         :: Text
                                     , outCbCreateTime :: UTCTime
                                     , outCbContent    :: OutMessageContent
                                     }

data OutCSMessage = OutCSMessage { outCSAccessToken :: Text
                                 , outCSTo          :: Text
                                 , outCSContent     :: OutMessageContent
                                 }

