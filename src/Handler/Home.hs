{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Data.Map.Strict
import Data.Aeson

import qualified Data.ByteString.Lazy as DBL

import Binding.Currency
import Binding.SessionHolding

import Binding.Ark (getArkHolding)
import Binding.Btc (getBtcHolding)
import Binding.Eth (getEthHolding)
import Binding.Xrp (getXrpHolding)

type NameAddresses = [NameAddress]

getHoldings :: [NameAddress]
getHoldings = [NameAddress { name = "ark", address = "AGDENwv5qXV2zfXQBh9nUdNesdX8Yyk7ow" },
               NameAddress { name = "eth", address = "0xf4d5aDe921b09d4A2e05f45D26483fA5735c802C" },
               NameAddress { name = "btc", address = "1GyT8JDvh5cTpsLTXbGtgNjiEgzAfoJpWf" },
               NameAddress { name = "xrp", address = "rf1BiGeXwwQoi8Z2ueFYTEXSwuJYfV2Jpn" }]

getNameAddresses :: Maybe Text -> [NameAddress]
getNameAddresses Nothing = []
getNameAddresses (Just jsonText) = maybe [] (\a -> a) ((decode . DBL.fromStrict . encodeUtf8) jsonText)

--test :: Maybe Text -> ByteString
--test Nothing = 
--test (Just jsonText) = maybe [] (\a -> a) ((decode . DBL.fromStrict . encodeUtf8) jsonText)

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

getHolding :: NameAddress -> IHolding
getHolding na = holdingMap ! (name na) $ address na

extractHoldings :: [NameAddress] -> [IHolding]
extractHoldings nas = (fmap) getHolding nas

extractBalances :: [IHolding] -> IO ([Maybe Float])
extractBalances hs = sequence $ (fmap) (\h -> getHoldingBalance h) hs

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    setSession "vals" $ (decodeUtf8 . DBL.toStrict . encode) getHoldings
    vals <- lookupSession "vals"
    holdings <- return $ extractHoldings (getNameAddresses vals)
    balances <- liftIO $ extractBalances holdings
    holdingTuples <- return $ zipWith (\h b -> (h, b)) holdings balances
    $(widgetFile "homepage")
    
