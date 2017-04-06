module Web.WeChat.XMLParse where
 
import           Control.Monad          (guard)

import           Data.Char              (isSpace)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Text.Read              (readMaybe)
import           Text.XML.Light

import           Web.WeChat.Types


textContent :: Element -> Text
textContent = T.pack . strContent'

-- strContent' is a version of strContent that ignores all-whitespace text nodes
-- when they aren't enclosed in CDATA[]
strContent' :: Element -> String
strContent' e = concat [ cd
                       | c <- onlyText $ elContent e
                       , let cd = cdData c
                       , not (all isSpace cd) || cdVerbatim c == CDataVerbatim
                       ]

tag :: Text -> QName
tag = unqual . T.unpack

readContent :: Read a => Element -> Maybe a
readContent = readMaybe . strContent'

intContent :: Element -> Maybe Integer
intContent = readContent

textTag :: Text -> Element -> Maybe Text
textTag txt = fmap textContent . findChild (tag txt)

readTag :: Read a => Text -> Element -> Maybe a
readTag txt = (>>= readContent) . findChild (tag txt)

parseInEncryptedMessage :: Element -> Maybe InEncryptedMessage
parseInEncryptedMessage elt =
  InEncryptedMessage <$> textTag "Encrypt" elt
                     <*> textTag "ToUserName" elt

parseInMessage' :: Element -> Maybe (Either InEncryptedMessage InMessage)
parseInMessage' elt =
  case parseInEncryptedMessage elt of
    Just encrypted -> return $ Left encrypted
    Nothing -> do
      inFrom       <- textTag "FromUserName" elt
      inTo         <- textTag "ToUserName" elt
      inCreateTime <- intContent =<< findChild (tag "CreateTime") elt

      -- If no tag, no biggie.            Just Nothing
      -- If a tag but no parse, fail.     Nothing
      -- If a tag and a parse, succeed.   Just (Just msgId)
      inMsgId      <- case findChild (tag "MsgId") elt of
                        Nothing -> return Nothing
                        Just msgIdTag -> fmap Just $ intContent msgIdTag

      msgType   <- T.toLower <$> textTag "MsgType" elt
      case inMsgId of
        Nothing -> guard (msgType == "event")
        Just{}  -> return ()

      inContent <- parseInMessageContent msgType elt

      return $ Right InMessage{..}

parseInMessageContent :: Text -> Element -> Maybe InMessageContent
parseInMessageContent "event"    = parseEventMessage
parseInMessageContent "text"     = parseTextMessage
parseInMessageContent "image"    = parseImageMessage
parseInMessageContent "voice"    = parseVoiceMessage
parseInMessageContent "video"    = parseVideoMessage
parseInMessageContent "location" = parseLocationMessage
parseInMessageContent "link"     = parseLinkMessage
parseInMessageContent _          = const Nothing

parseEventMessage,
  parseTextMessage,
  parseImageMessage,
  parseVoiceMessage,
  parseVideoMessage,
  parseLocationMessage,
  parseLinkMessage :: Element -> Maybe InMessageContent

parseTextMessage elt = InText <$> textTag "Content" elt
parseImageMessage elt = do
  inPicURL <- textTag "PicUrl" elt
  inMediaID <- textTag "MediaId" elt
  return InImage{..}
parseVoiceMessage elt = do
  inMediaID <- textTag "MediaId" elt
  inFormat <- textTag "Format" elt
  case textTag "Recognition" elt of
    Nothing -> return InAudio{..}
    Just inRecognition -> return InSpeechRecognition{..}
parseVideoMessage elt = do
  inMediaID <- textTag "MediaId" elt
  inThumbMediaID <- textTag "ThumbMediaId" elt
  return InVideo{..}
parseLocationMessage elt = do
  inLat <- readTag "Location_X" elt
  inLon <- readTag "Location_Y" elt
  inScale <- readTag "Scale" elt
  inLabel <- textTag "Label" elt
  return InLocation{..}
parseLinkMessage elt = do
  inTitle <- textTag "Title" elt
  inDescription <- textTag "Description" elt
  inUrl <- textTag "Url" elt
  return InLink{..}

parseEventMessage elt = do
  event <- textTag "Event" elt
  case T.toLower event of
    "subscribe"   -> parseEventSubscribe elt
    "unsubscribe" -> return $ InEvent Unsubscribe
    "scan"        -> parseEventScan elt
    "location"    -> parseEventLocation elt
    "click"       -> parseEventClick elt
    "view"        -> parseEventRedirect elt
    "scancode_push"    -> parseEventScanOther QRScanPush elt -- makes user go to website or makes it show text in a different window (like when scanning QR outside of wechat, kindof)
    "scancode_waitmsg" -> parseEventScanOther QRScanWait elt -- only sends to backend for backend to react to
    "pic_sysphoto"       -> parseEventPicOther PicSysPhoto elt     -- Let user take and send picture with camera
    "pic_photo_or_album" -> parseEventPicOther PicPhotoOrAlbum elt -- Let user take and send picture with camera or send pic from album
    "pic_weixin"         -> parseEventPicOther PicWeixin elt       -- Let user send pic from album
    "location_select"    -> parseEventLocationSelect elt -- Ask user for coordinates
    _ -> Nothing

parseEventScanOther :: (Text -> Text -> InEvent) -> Element -> Maybe InMessageContent
parseEventPicOther  :: (Text -> Int -> [Text] -> InEvent) -> Element -> Maybe InMessageContent

parseEventSubscribe,
  parseEventScan,
  parseEventLocation,
  parseEventLocationSelect,
  parseEventClick,
  parseEventRedirect :: Element -> Maybe InMessageContent

parseEventSubscribe elt = do
  case textTag "EventKey" elt of
    Nothing -> return $ InEvent Subscribe
    Just "" -> return $ InEvent Subscribe
    Just key -> do
      eventTicket <- textTag "Ticket" elt
      let eventKey = fromMaybe key $ T.stripPrefix "qrscene_" key
      return $ InEvent QRSubscribe{..}
parseEventScan elt = do
  eventKey    <- textTag "EventKey" elt
  eventTicket <- textTag "Ticket" elt
  return $ InEvent QRScan{..}
parseEventScanOther dataType elt = do
  eventKey    <- textTag "EventKey" elt
  eventResult <- textTag "ScanResult" =<< findChild (tag "ScanCodeInfo") elt
  return $ InEvent $ dataType eventKey eventResult
parseEventPicOther dataType elt = do
  eventKey    <- textTag "EventKey" elt
  spiElt      <- findChild (tag "SendPicsInfo") elt
  eventCount  <- readTag "Count" spiElt :: Maybe Int
  eventPicList <- mapM (textTag "PicMd5Sum") . findChildren (tag "item") =<< findChild (tag "PicList") spiElt
  return $ InEvent $ dataType eventKey eventCount eventPicList
parseEventLocationSelect elt = do
  eventKey     <- textTag "EventKey" elt
  sliElt       <- findChild (tag "SendLocationInfo") elt
  eventLat     <- readTag "Location_X" sliElt
  eventLon     <- readTag "Location_Y" sliElt
  eventScale   <- readTag "Scale" sliElt
  eventLabel   <- textTag "Label" sliElt -- Address most of the time (at least in NL)
  let eventPOIName = textTag "Poiname" sliElt -- Name of POI or "[Location]"
  return $ InEvent LocationSelect{..}
parseEventLocation elt = do
  eventLat <- readTag "Latitude" elt
  eventLon <- readTag "Longitude" elt
  eventPrecision <- readTag "Precision" elt
  return $ InEvent Location{..}
parseEventClick elt = do
  eventKey <- textTag "EventKey" elt
  return $ InEvent Click{..}
parseEventRedirect elt = do
  eventKey <- textTag "EventKey" elt
  let eventMenuId = readTag "MenuId" elt :: Maybe Integer
  return $ InEvent Redirect{..}
-- VIEW also gets a <MenuId></MenuId> tag when clicked from the menu... needed, yes/no?
