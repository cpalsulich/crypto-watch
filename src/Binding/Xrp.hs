{-# LANGUAGE OverloadedStrings #-}
module Binding.Xrp where

import Binding.Currency
import Control.Lens
import Data.Aeson.Lens (key, nth, _String)
import Data.Text
import Network.Wreq (responseBody)

import Common.Http (get)

data Xrp = Xrp
  { name :: Text,
    balance :: IO (Maybe Double),
    address :: Text
  }

instance Holding (Xrp) where
  getName = name
  getAddress = address
  getBalance = balance

getXrpHolding :: Text -> Xrp
getXrpHolding addr
  = Xrp { name = "Xrp",
          balance = getXrpBalance,
          address = addr }
  where getXrpBalance :: IO (Maybe Double)
        getXrpBalance = do
          r <- get $ unpack $ intercalate "" ["https://data.ripple.com/v2/accounts/", addr, "/balances/?currency=XRP"]
          return $ (read . unpack) <$> (r ^? responseBody . key "balances" . nth 0 . key "value" .  _String)
