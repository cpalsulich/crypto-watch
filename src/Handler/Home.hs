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
import Binding.Currency
import Binding.SessionHolding

getHoldings :: [NameAddress]
getHoldings = [NameAddress { name = "ark", address = "AGDENwv5qXV2zfXQBh9nUdNesdX8Yyk7ow" }]

getHoldingName :: IHolding -> Text
getHoldingName (MkHolding a) = getName a

getHoldingAddress :: IHolding -> Text
getHoldingAddress (MkHolding a) = getAddress a

getHoldingBalance :: IHolding -> IO (Maybe Text)
getHoldingBalance (MkHolding a) = getBalance a

holdingMap :: Map Text (Text -> IHolding)
holdingMap = Data.Map.Strict.fromList [("ark", (\addr -> packHolding $ getArkHolding addr))]

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    holdings <- return $ fmap (\na -> holdingMap ! (name na) $ address na) getHoldings
    balances <- liftIO $ sequence $ fmap (\h -> getHoldingBalance h) holdings
    holdingTuples <- return $ zipWith (\h b -> (h, b)) holdings balances
    $(widgetFile "homepage")
    --arkBalance <- liftIO
    --  $ getBalance arkHolding (getAddress arkHolding)
    
