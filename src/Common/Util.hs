module Common.Util where

import Data.Aeson
import Data.Text
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as DBL

import Binding.SessionHolding

getNameAddresses :: Maybe Text -> [NameAddress]
getNameAddresses Nothing = []
getNameAddresses (Just jsonText) = maybe [] id $ fromJson jsonText

fromJson :: FromJSON a => Text -> Maybe a
fromJson = (decode . DBL.fromStrict . encodeUtf8)

toJson :: ToJSON a => a -> Text
toJson = (decodeUtf8 . DBL.toStrict . encode)
