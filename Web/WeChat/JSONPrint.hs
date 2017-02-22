{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.WeChat.JSONPrint where

import           Data.Aeson
import           Data.Aeson.Types (Pair,typeMismatch)
import qualified Data.Text as T
import           Data.Maybe (maybeToList)

import Web.WeChat.Types

instance ToJSON OutCSMessage where
  toJSON (OutCSMessage to cont) = object $
    [ "touser" .= to ] ++
    outJSON cont

mediaJSON :: T.Text -> [Pair] -> [Pair]
mediaJSON typ cont = ["msgtype" .= typ, typ .= object cont]

mbJSON :: ToJSON a => T.Text -> Maybe a -> [Pair]
mbJSON lbl = maybeToList . fmap (\val -> lbl .= val)

outJSON :: OutMessageContent -> [Pair]
outJSON (OutText cont) = mediaJSON "text" ["content" .= cont]
outJSON (OutImage mId) = mediaJSON "image" ["media_id" .= mId]
outJSON (OutAudio mId) = mediaJSON "voice" ["media_id" .= mId]
outJSON (OutVideo mId thMId) = mediaJSON "video" ["media_id" .= mId, "thumb_media_id" .= thMId]
outJSON (OutMusic mbTitle mbDescr mbURL mbHQURL mId) = mediaJSON "music" $
  mbJSON "title" mbTitle ++
  mbJSON "description" mbDescr ++
  mbJSON "musicurl" mbURL ++
  mbJSON "hqmusicurl" mbHQURL ++
  ["thumb_media_id" .= mId]
outJSON (OutRich articles) = mediaJSON "news" ["articles" .= articles]

instance ToJSON OutRichArticle where
  toJSON (OutRichArticle mbTitle mbDescr mbPicURL mbURL) =
    object $ mbJSON "title" mbTitle ++
             mbJSON "description" mbDescr ++
             mbJSON "url" mbURL ++
             mbJSON "picurl" mbPicURL

instance FromJSON ErrorResponse where
  parseJSON (Object o) =
    ErrorResponse <$> o .: "errcode"
                  <*> o .: "errmsg"
                  <*> o .:? "msgid"
  parseJSON wat = typeMismatch "ErrorResponse" wat

instance FromJSON MultimediaTransferResponse where
  parseJSON (Object o) =
    MultimediaTransferResponse <$> o .: "type"
                               <*> o .: "media_id"
                               <*> o .: "created_at"
  parseJSON wat = typeMismatch "MultimediaTransferResponse" wat

instance FromJSON AccessTokenResponse where
  parseJSON (Object o) =
    AccessTokenResponse <$> o .: "access_token"
                        <*> o .: "expires_in"
  parseJSON wat = typeMismatch "AccessTokenResponse" wat
