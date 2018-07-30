{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Binding.Eth where

import Binding.Currency
import Control.Lens
import Data.Aeson.Lens (key, _String)
import Data.Text
import GHC.Generics
import Network.Wreq (responseBody)
import Data.Aeson
import Numeric

import Common.Http (post)

data Eth = Eth
  { name :: Text,
    balance :: IO (Maybe Double),
    address :: Text
  }

instance Holding (Eth) where
  getName = name
  getAddress = address
  getBalance = balance

data EthRequest = EthRequest
  { jsonrpc :: Text,
    id :: Integer,
    method :: Text,
    params :: [Text] }
  deriving Generic

instance ToJSON EthRequest where
    toEncoding = genericToEncoding defaultOptions

getEthHolding :: Text -> Eth
getEthHolding addr
  = Eth { name = "Eth",
          balance = getEthBalance,
          address = addr }
  where getEthBalance :: IO (Maybe Double)
        getEthBalance = do
          r <- post "https://mainnet.infura.io/" $ toJSON $ EthRequest { jsonrpc = "2.0",
                                                                         Binding.Eth.id = 1,
                                                                         method = "eth_getBalance",
                                                                         params = [addr, "latest"] }
          return
            $ ((/1e18) . fst . ((flip (!!)) 0) . readHex . unpack . (Data.Text.drop 2))
            <$> (r ^? responseBody . key "result" . _String)

