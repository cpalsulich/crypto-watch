{-# LANGUAGE OverloadedStrings #-}
module Binding.Bch where

import Binding.Currency
import Network.Wreq (responseBody)
import Control.Exception as E
import Control.Lens
import Data.Aeson.Lens (key, _Double)
import Data.Text

import Common.Http (get)

data Bch = Bch
  { name :: Text,
    balance :: IO (Maybe Double),
    address :: Text
  }

instance Holding (Bch) where
  getName = name
  getAddress = address
  getBalance = balance

getBchHolding :: Text -> Bch
getBchHolding addr
  = Bch { name = "Bch",
          balance = getBchBalance,
          address = addr }
    where
      getBchBalance :: IO (Maybe Double)
      getBchBalance = do
        r <- get ("https://explorer.bitcoin.com/api/btc/addr/" ++ (unpack addr))
        return $ r ^? responseBody . key "balance" . _Double
