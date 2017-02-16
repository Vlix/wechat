module Web.WeChat.XMLPrint where

import           Data.Maybe (maybeToList)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import           Text.XML.Light

import           Web.WeChat.Types


tag :: T.Text -> QName
tag txt = unqual (T.unpack txt)

mkTag :: T.Text -> [Content] -> Element
mkTag lbl cont = Element (tag lbl) [] cont Nothing

eltTag :: T.Text -> [Element] -> Element
eltTag lbl elts = mkTag lbl (map Elem elts)

stringTag :: T.Text -> String -> Element
stringTag lbl str = mkTag lbl [Text $ CData CDataVerbatim str Nothing]

textTag :: T.Text -> T.Text -> Element
textTag lbl txt = stringTag lbl (T.unpack txt)

showTag :: Show a => T.Text -> a -> Element
showTag lbl thing = stringTag lbl (show thing)

printOutMessage :: OutCallbackMessage -> T.Text
printOutMessage = T.pack . showElement . xmlEnvelope . printOutMessage'
  where xmlEnvelope :: [Element] -> Element
        xmlEnvelope elts = Element (tag "xml") [] (map Elem elts) Nothing

printOutMessage' :: OutCallbackMessage -> [Element]
printOutMessage' (OutMessage from to ct cont) =
  [ textTag "ToUserName" to
  , textTag "FromUserName" from
  , showTag "CreateTime" (dispTime ct)
  ] ++ printOutContent cont
  where dispTime :: UTCTime -> Integer
        dispTime = floor . utcTimeToPOSIXSeconds

outXML :: Text -> [Element] -> [Element]
outXML typ elts = textTag "MsgType" typ : elts

mediaXML :: Text -> [Element] -> [Element]
mediaXML typ elts = outXML (T.toLower typ) [eltTag typ elts]

mbTag :: (a -> b) -> Maybe a -> [b]
mbTag f = maybeToList . fmap f

printOutContent :: OutMessageContent -> [Element]
printOutContent (OutText txt) = outXML "text" [textTag "Content" txt]
printOutContent (OutImage mID) = mediaXML "Image" [textTag "MediaId" mID]
printOutContent (OutAudio mID) = mediaXML "Voice" [textTag "MediaId" mID]
printOutContent (OutVideo mId thMId) = mediaXML "Video"
  [textTag "MediaId" mId, textTag "ThumbMediaId" thMId]
printOutContent (OutMusic mbTitle mbDescr mbURL mbHQURL mID) =
  outXML "music" $ eltTag "Music" $
    mbTag (textTag "Title") mbTitle ++
    mbTag (textTag "Description") mbDescr ++
    mbTag (textTag "MusicUrl") mbURL ++
    mbTag (textTag "HQMusicUrl") mbHQURL ++
    [textTag "ThumbMediaId" mID]












