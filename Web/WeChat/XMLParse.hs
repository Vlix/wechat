module Web.WeChat.XMLParse where

import           Control.Monad (guard)

import qualified Data.Text as T
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Text.Read (readMaybe)
import           Text.XML.Light
import           Text.XML.Light.Lexer (XmlSource)

import           Web.WeChat.Types


textContent :: Element -> T.Text
textContent = T.pack . strContent

tag :: T.Text -> QName
tag txt = unqual (T.unpack txt)

readContent :: Read a => Element -> Maybe a
readContent = readMaybe . strContent

intContent :: Element -> Maybe Integer
intContent = readContent

textTag :: T.Text -> Element -> Maybe T.Text
textTag txt elt = textContent <$> findChild (tag txt) elt

readTag :: Read a => T.Text -> Element -> Maybe a
readTag txt elt = readContent =<< findChild (tag txt) elt

parseInMessage :: XmlSource s => s -> Maybe InMessage
parseInMessage s = parseXMLDoc s >>= parseInMessage'

parseInMessage' :: Element -> Maybe InMessage
parseInMessage' elt = do
  inFrom       <- textTag "FromUserName" elt
  inTo         <- textTag "ToUserName" elt
  inCreateTime <- (posixSecondsToUTCTime . realToFrac) <$> (intContent =<< findChild (tag "CreateTime") elt)

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

  return InMessage{..}

parseInMessageContent :: T.Text -> Element -> Maybe InMessageContent
parseInMessageContent "event"    = parseEventMessage
parseInMessageContent "text"     = parseTextMessage
parseInMessageContent "image"    = parseImageMessage
parseInMessageContent "voice"    = parseVoiceMessage
parseInMessageContent "video"    = parseVideoMessage
parseInMessageContent "location" = parseLocationMessage
parseInMessageContent "link"     = parseLinkMessage
parseInMessageContent _          = const Nothing

parseEventMessage, parseTextMessage, parseImageMessage, parseVoiceMessage, parseVideoMessage, parseLocationMessage, parseLinkMessage :: Element -> Maybe InMessageContent

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
    "unsubscribe" -> return InEventUnsubscribe
    "scan"        -> parseEventScan elt
    "location"    -> parseEventLocation elt
    "click"       -> parseEventClick elt
    "view"        -> parseEventRedirect elt
    _             -> Nothing

parseEventSubscribe, parseEventScan, parseEventLocation, parseEventClick, parseEventRedirect :: Element -> Maybe InMessageContent

parseEventSubscribe elt = do
  case textTag "EventKey" elt of
    Just inEventKey -> do
      inTicket <- textTag "Ticket" elt
      return InEventQRSubscribe{..}
    Nothing -> return InEventSubscribe
parseEventScan elt = do
  inEventKey <- textTag "EventKey" elt
  inTicket <- textTag "Ticket" elt
  return InEventScan{..}
parseEventLocation elt = do
  inLat <- readTag "Latitude" elt
  inLon <- readTag "Longitude" elt
  inPrecision <- readTag "Precision" elt
  return InEventLocation{..}
parseEventClick elt = do
  inEventKey <- textTag "EventKey" elt
  return InEventClick{..}
parseEventRedirect elt = do
  inEventKey <- textTag "EventKey" elt
  return InEventRedirect{..}

