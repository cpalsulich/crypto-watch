{-# LANGUAGE OverloadedStrings #-}
module Binding.Ark where

import Binding.Currency
import Network.Wreq (responseBody, get)
import Control.Lens
import Data.Aeson.Lens (key, _String)
import Data.Text

data Ark = Ark
  { name :: Text,
    balance :: Address -> IO (Maybe Text),
    address :: Address
  }

instance Holding (Ark) where
  getName = name
  getAddress = address
  getBalance = balance

getArkHolding :: Address -> Ark
getArkHolding addr
  = Ark { name = "Ark",
          balance = _getArkBalance,
          address = addr }
    where
      _getArkBalance :: Address -> IO (Maybe Text)
      _getArkBalance a = do
        r <- get ("https://explorer.ark.io:8443/api/accounts?address=" ++ (unpack a))
        return $ (r ^? responseBody . key "account" . key "balance" . _String)
