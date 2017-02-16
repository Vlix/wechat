{-# LANGUAGE FlexibleInstances #-}

module Web.WeChat.Instances where


import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BS8
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text              (Text)
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text.Encoding     as TE
import           Data.Word



class ToByteString a where
  toByteString :: a -> ByteString

instance ToByteString ByteString where
  toByteString = id

instance ToByteString LBS.ByteString where
  toByteString = LBS.toStrict

instance ToByteString Text where
  toByteString = TE.encodeUtf8

instance ToByteString LT.Text where
  toByteString = toByteString . LTE.encodeUtf8

instance ToByteString [Char] where
  toByteString = BS8.pack

instance ToByteString [Word8] where
  toByteString = BS.pack
