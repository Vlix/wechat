module Web.WeChat.Types where


import           Data.Aeson
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
   = InText  { inTextContent :: Text }
   | InImage { inPicURL :: URL, inMediaID :: MediaID }
   | InAudio { inMediaID :: MediaID, inFormat :: Text }
   | InVideo { inMediaID :: MediaID, inThumbMediaID :: MediaID }
   | InLocation { inLat :: Double, inLon :: Double, inScale :: Int, inLabel :: Text}
   | InLink  { inTitle :: Text, inDescription :: Text, inUrl :: URL }
   | InEvent { inEvent :: InEvent }
   | InSpeechRecognition { inMediaID :: MediaID, inFormat :: Text, inRecognition :: Text }
  deriving (Eq, Show)

data InEvent
  = Subscribe
  | Unsubscribe
  | QRSubscribe { eventKey :: Text, eventTicket :: Text } -- QR scanned outside of channel X directed towards channel X (combined with Subscribe)
  | QRScan      { eventKey :: Text, eventTicket :: Text } -- QR scanned outside of channel X directed towards channel X
  | QRScanPush  { eventKey :: Text, eventResult :: Text } -- QR scanned inside of channel (through Menu button) and forward to site or show text
  | QRScanWait  { eventKey :: Text, eventResult :: Text } -- QR scanned inside of channel (through Menu button) and wait for backend to do something
  | PicSysPhoto     { eventKey :: Text, eventCount :: Int, eventPicList :: [Text] } -- Picture(s) sent to channel from menu button (from camera)
  | PicPhotoOrAlbum { eventKey :: Text, eventCount :: Int, eventPicList :: [Text] } -- Picture(s) sent to channel from menu button (from camera or album)
  | PicWeixin       { eventKey :: Text, eventCount :: Int, eventPicList :: [Text] } -- Picture(s) sent to channel from menu button (from album)
  | LocationSelect  { eventKey     :: Text
                    , eventLat     :: Double
                    , eventLon     :: Double
                    , eventScale   :: Int
                    , eventLabel   :: Text
                    , eventPOIName :: Maybe Text } -- Location sent to channel from menu button
  | Click       { eventKey :: Text }
  | Redirect    { eventKey :: Text, eventMenuId :: Maybe Integer }
  | Location    { eventLat :: Double, eventLon :: Double, eventPrecision :: Double }
  deriving (Eq, Show, Read)

data InMessage = InMessage
  { inFrom       :: Text
  , inTo         :: Text
  , inCreateTime :: Integer
  , inContent    :: InMessageContent
  , inMsgId      :: Maybe Integer
  } deriving (Eq, Show)

data InEncryptedMessage = InEncryptedMessage
  { inEncrypt :: Text
  , inEncTo   :: Text
  } deriving (Eq, Show)


--------------
-- OUTGOING --
--------------

data OutMessageContent
   = OutText  { outTextContent :: Text }
   | OutImage { outMediaID     :: MediaID }
   | OutAudio { outMediaID     :: MediaID }
   | OutVideo { outMediaID     :: MediaID, outThumbMediaID :: MediaID }
   | OutMusic { outTitle        :: Maybe Text
              , outDescription  :: Maybe Text
              , outMusicURL     :: Maybe URL
              , outHQMusicURL   :: Maybe URL
              , outThumbMediaID :: MediaID }
   | OutRich  { outArticles     :: [OutRichArticle] }
  deriving (Eq, Show)

data OutRichArticle = OutRichArticle { outArticleTitle       :: Maybe Text
                                     , outArticleDescription :: Maybe Text
                                     , outPicURL             :: Maybe URL
                                     , outURL                :: Maybe URL
                                     }
  deriving (Eq, Show)

data OutCallbackMessage = OutMessage { outCbFrom       :: Text
                                     , outCbTo         :: Text
                                     , outCbCreateTime :: Integer
                                     , outCbContent    :: OutMessageContent
                                     }
  deriving (Eq, Show)

data OutCSMessage = OutCSMessage { outCSTo      :: Text
                                 , outCSContent :: OutMessageContent
                                 }
  deriving (Eq, Show)


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

data MultimediaPermanentTransferResponse = MultimediaPermanentTransferResponse
  { multimedia_perm_id  :: Text
  , multimedia_perm_url :: Text
  }

data AccessTokenResponse = AccessTokenResponse { accessToken :: Text, accessTokenExpires :: Int }


instance FromJSON ErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \o ->
    ErrorResponse <$> o .: "errcode"
                  <*> o .: "errmsg"
                  <*> o .:? "msgid"

instance FromJSON MultimediaTransferResponse where
  parseJSON = withObject "MultimediaTransferResponse" $ \o ->
    MultimediaTransferResponse <$> o .: "type"
                               <*> o .: "media_id"
                               <*> o .: "created_at"

instance FromJSON MultimediaPermanentTransferResponse where
  parseJSON = withObject "MultimediaPermanentTransferResponse" $ \o ->
    MultimediaPermanentTransferResponse <$> o .: "media_id"
                                        <*> o .: "url"

instance FromJSON AccessTokenResponse where
  parseJSON = withObject "AccessTokenResponse" $ \o ->
    AccessTokenResponse <$> o .: "access_token"
                        <*> o .: "expires_in"
