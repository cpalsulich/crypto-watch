{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Data.List
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
getHoldings = [NameAddress { name = "ark", Binding.SessionHolding.address = "AGDENwv5qXV2zfXQBh9nUdNesdX8Yyk7ow" },
               NameAddress { name = "eth", Binding.SessionHolding.address = "0xf4d5aDe921b09d4A2e05f45D26483fA5735c802C" },
               NameAddress { name = "btc", Binding.SessionHolding.address = "1GyT8JDvh5cTpsLTXbGtgNjiEgzAfoJpWf" },
               NameAddress { name = "xrp", Binding.SessionHolding.address = "rf1BiGeXwwQoi8Z2ueFYTEXSwuJYfV2Jpn" }]

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

optionMap :: Map Text (Text -> IHolding)
optionMap = Data.Map.Strict.fromList [("ark", (\addr -> packHolding $ getArkHolding addr)),
                                       ("eth", (\addr -> packHolding $ getEthHolding addr)),
                                       ("btc", (\addr -> packHolding $ getBtcHolding addr)),
                                       ("xrp", (\addr -> packHolding $ getXrpHolding addr))]

getHolding :: NameAddress -> IHolding
getHolding na = optionMap ! (name na) $ Binding.SessionHolding.address na

extractHoldings :: [NameAddress] -> [IHolding]
extractHoldings nas = (fmap) getHolding nas

extractBalances :: [IHolding] -> IO ([Maybe Float])
extractBalances hs = sequence $ (fmap) (\h -> getHoldingBalance h) hs

currencyChoiceAForm :: AForm Handler NameAddress
currencyChoiceAForm = NameAddress
    <$> areq (selectFieldList (Data.List.zipWith (\a b -> (a, b)) (Data.Map.Strict.keys optionMap) (Data.Map.Strict.keys optionMap))) "" Nothing
    <*> areq textField "Address" Nothing

currencyChoiceForm :: Html -> MForm Handler (FormResult NameAddress, Widget)
currencyChoiceForm = renderTable currencyChoiceAForm

getHomeR :: Handler Html
getHomeR = do
  (optionWidget, enctype) <- generateFormPost currencyChoiceForm
  defaultLayout $ do
    vals <- lookupSession "vals"
    holdings <- return $ extractHoldings (getNameAddresses vals)
    balances <- liftIO $ extractBalances holdings
    holdingTuples <- return $ Data.List.zipWith (\h b -> (h, b)) holdings balances
    $(widgetFile "homepage")
--    setSession "vals" $ (decodeUtf8 . DBL.toStrict . encode) getHoldings

postHomeR :: Handler Html
postHomeR = do
  ((result, _), _) <- runFormPost currencyChoiceForm
  vals <- lookupSession "vals"
  case result of
    FormSuccess choice ->
      setSession "vals" $ (decodeUtf8 . DBL.toStrict . encode) ([choice] <> (getNameAddresses vals))
  redirect HomeR
  
deleteHomeR :: Handler Html
deleteHomeR = do
  choice <- requireJsonBody :: Handler NameAddress
  vals <- lookupSession "vals"
  $(logInfo) $ pack $ show choice
  $(logInfo) $ pack $ show $ getNameAddresses vals
  setSession "vals" $
    (decodeUtf8 . DBL.toStrict . encode)
    (Data.List.filter
      (\na -> ((toLower $ name choice) /= (name na)) || ((address choice /= (address na))))
      (getNameAddresses vals))
  redirect HomeR
