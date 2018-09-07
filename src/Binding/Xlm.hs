{-# LANGUAGE OverloadedStrings #-}
module Binding.Xlm where

import Binding.Currency
import Control.Lens
import Control.Monad
import Data.Aeson.Lens (_Array, key, _Object, _String)
import Data.ByteString.Lazy
import Data.Text 
import Data.Vector (find, fromList)
import Network.Wreq (Response, responseBody)

import Common.Http (get)

data Xlm = Xlm
  { name :: Text,
    balance :: IO (Maybe Double),
    address :: Text
  }

instance Holding (Xlm) where
  getName = name
  getAddress = address
  getBalance = balance

getXlmHolding :: Text -> Xlm
getXlmHolding addr
  = Xlm { name = "Xlm",
          balance = getXlmBalance,
          address = addr }
    where
      getXlmBalance :: IO (Maybe Double)
      getXlmBalance = do
        r <- get ("https://stellarchain.io/address/" ++ (Data.Text.unpack addr))
        return
          $ (read . Data.Text.unpack)
          <$> balanceObject r ^? key "balance" . _String
      balanceObject :: Response ByteString -> Text
      balanceObject r
        = maybe "" id
          $ maybe Nothing (\x -> x ^? key "asset_type" . _String)
          $ (join (Data.Vector.find (\x -> x ^? key "asset_type" . _String == Just "native")
                   <$> r ^? responseBody . key "balances" . _Array))
