{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Data.Map.Strict
--import qualified Web.ClientSession   as CS

import Binding.Ark (getArkHolding)
import Binding.Btc (getBtcHolding)
import Binding.Currency
import Binding.Eth (getEthHolding)
import Binding.SessionHolding
import Binding.Xrp (getXrpHolding)

getHoldings :: [NameAddress]
getHoldings = [NameAddress { name = "ark", address = "AGDENwv5qXV2zfXQBh9nUdNesdX8Yyk7ow" },
               NameAddress { name = "eth", address = "0xf4d5aDe921b09d4A2e05f45D26483fA5735c802C" },
               NameAddress { name = "btc", address = "1GyT8JDvh5cTpsLTXbGtgNjiEgzAfoJpWf" },
               NameAddress { name = "xrp", address = "rf1BiGeXwwQoi8Z2ueFYTEXSwuJYfV2Jpn" }]

getHoldingName :: IHolding -> Text
getHoldingName (MkHolding a) = getName a

getHoldingAddress :: IHolding -> Text
getHoldingAddress (MkHolding a) = getAddress a

getHoldingBalance :: IHolding -> IO (Maybe Float)
getHoldingBalance (MkHolding a) = getBalance a

holdingMap :: Map Text (Text -> IHolding)
holdingMap = Data.Map.Strict.fromList [("ark", (\addr -> packHolding $ getArkHolding addr)),
                                       ("eth", (\addr -> packHolding $ getEthHolding addr)),
                                       ("btc", (\addr -> packHolding $ getBtcHolding addr)),
                                       ("xrp", (\addr -> packHolding $ getXrpHolding addr))]

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    holdings <- return $ fmap (\na -> holdingMap ! (name na) $ address na) getHoldings
    balances <- liftIO $ sequence $ fmap (\h -> getHoldingBalance h) holdings
    holdingTuples <- return $ zipWith (\h b -> (h, b)) holdings balances
    $(widgetFile "homepage")
    --arkBalance <- liftIO
    --  $ getBalance arkHolding (getAddress arkHolding)
    
