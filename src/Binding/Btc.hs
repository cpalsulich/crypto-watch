{-# LANGUAGE OverloadedStrings #-}
module Binding.Btc where

import Binding.Currency
import Control.Lens
import Data.Aeson.Lens (key, _Integer)
import Data.Text
import Network.Wreq (responseBody, get)

data Btc = Btc
  { name :: Text,
    balance :: IO (Maybe Double),
    address :: Text
  }

instance Holding (Btc) where
  getName = name
  getAddress = address
  getBalance = balance

getBtcHolding :: Text -> Btc
getBtcHolding addr
  = Btc { name = "Btc",
          balance = getBtcBalance,
          address = addr }
  where getBtcBalance :: IO (Maybe Double)
        getBtcBalance = do
          r <- get $ unpack $ intercalate "" ["https://blockchain.info/address/", addr, "?format=json"]
          return $ ((/1e8) . fromIntegral) <$> (r ^? responseBody . key "final_balance" . _Integer)
