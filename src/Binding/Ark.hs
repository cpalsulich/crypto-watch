{-# LANGUAGE OverloadedStrings #-}
module Binding.Ark where

import Binding.Currency
import Network.Wreq (responseBody, get)
import Control.Lens
import Data.Aeson.Lens (key, _String)
import Data.Text

data Ark = Ark
  { name :: Text,
    balance :: IO (Maybe Double),
    address :: Text
  }

instance Holding (Ark) where
  getName = name
  getAddress = address
  getBalance = balance

getArkHolding :: Text -> Ark
getArkHolding addr
  = Ark { name = "Ark",
          balance = getArkBalance,
          address = addr }
    where
      getArkBalance :: IO (Maybe Double)
      getArkBalance = do
        r <- get ("https://explorer.ark.io:8443/api/accounts?address=" ++ (unpack addr))
        return $ ((/10e7) . read . unpack) <$> (r ^? responseBody . key "account" . key "balance" . _String)
