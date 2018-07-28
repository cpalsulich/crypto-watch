{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module External.Coinmarketcap where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Map.Internal
import Data.Text
import GHC.Generics
import Network.Wreq (responseBody, get)
import Yesod.Core.Handler

type Ticker = Map String TickerCurrency

--instance FromJSON Ticker
--instance ToJSON Ticker where
--  toEncoding = genericToEncoding defaultOptions

data TickerCurrency = TickerCurrency {
  id :: Integer,
  name :: String,
  symbol :: String,
  quotes :: Map String TickerQuote,
  last_updated :: Integer }
  deriving Generic

instance FromJSON TickerCurrency
instance ToJSON TickerCurrency where
  toEncoding = genericToEncoding defaultOptions

data TickerQuote = TickerQuote {
  price :: Float,
  percent_change_24h :: Float }
  deriving Generic

instance FromJSON TickerQuote
instance ToJSON TickerQuote where
  toEncoding = genericToEncoding defaultOptions

getTicker :: IO (Maybe Ticker)
getTicker = do
  r <- get "https://api.coinmarketcap.com/v2/ticker/"
  tickerObject <- return $ r ^? responseBody . key "data" . _Value
  return $ (getResult . fromJSON . maybe Null Prelude.id) tickerObject
    where
      getResult :: Result Ticker -> Maybe Ticker
      getResult (Error _) = Nothing
      getResult (Success val) = Just val
  
