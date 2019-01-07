{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Prelude as P

-- import Data.Aeson
import Data.List
import Data.Map.Internal
import Import
import Yesod.Form.Bootstrap4

import Binding.Currency
import Binding.SessionHolding as BS

import Common.Util (getNameAddresses, toJson)

import Binding.Ark (getArkHolding)
import Binding.Bch (getBchHolding)
import Binding.Btc (getBtcHolding)
import Binding.Eth (getEthHolding)
import Binding.Xrp (getXrpHolding)
import External.Coinmarketcap as EC

type NameAddresses = [NameAddress]

currency :: Text
currency = "USD"

getTickerBalance :: Maybe Ticker -> Text -> Maybe Double -> Maybe Double
getTickerBalance mTicker val mAmount = mTicker
  >>= (\x -> Just $ elems x)
  >>= (\currencies -> Data.List.find (\x -> (toLower val) == (pack P.. toLower P.. EC.symbol $ x)) currencies)
  >>= (Data.Map.Internal.lookup $ unpack currency) P.. quotes
  >>= (\x -> maybe Nothing (\amount -> Just $ (price x) * amount) mAmount)

getTickerPrice :: Maybe Ticker -> Text -> Maybe Double
getTickerPrice mTicker val = mTicker
  >>= (\x -> Just $ elems x)
  >>= (\currencies -> Data.List.find (\x -> (toLower val) == (pack P.. toLower P.. EC.symbol $ x)) currencies)
  >>= (Data.Map.Internal.lookup $ unpack currency) P.. quotes
  >>= (\x -> Just $ price x)

getHoldingName :: IHolding -> Text
getHoldingName (MkHolding a) = getName a

getHoldingAddress :: IHolding -> Text
getHoldingAddress (MkHolding a) = getAddress a

getHoldingBalance :: IHolding -> IO (Maybe Double)
getHoldingBalance (MkHolding a) = getBalance a

optionMap :: Map Text (Text -> IHolding)
optionMap = Data.Map.Internal.fromList [("ark", (\addr -> packHolding $ getArkHolding addr)),
                                        ("eth", (\addr -> packHolding $ getEthHolding addr)),
                                        ("btc", (\addr -> packHolding $ getBtcHolding addr)),
                                        ("xrp", (\addr -> packHolding $ getXrpHolding addr)),
                                        ("bch", (\addr -> packHolding $ getBchHolding addr))]

getHolding :: NameAddress -> IHolding
getHolding na = optionMap ! (BS.name na) $ BS.address na

extractHoldings :: [NameAddress] -> [IHolding]
extractHoldings nas = (fmap) getHolding nas

extractBalances :: [IHolding] -> IO ([Maybe Double])
extractBalances hs = mapConcurrently (\h -> getHoldingBalance h) hs

currencyChoiceAForm :: AForm Handler NameAddress
currencyChoiceAForm
  = NameAddress
    <$> (areq (selectFieldList (Data.List.zipWith (\a b -> (a, b))
                                (Data.Map.Internal.keys optionMap)
                                (Data.Map.Internal.keys optionMap)))
          (bfs ("" :: Text))
          Nothing)
    <*> (areq textField
         (FieldSettings {fsName = Nothing,
                         fsAttrs = [("placeholder" :: Text, "Address" :: Text),
                                    ("class" :: Text, "col-sm form-control" :: Text)],
                         fsLabel = SomeMessage ("" :: Text),
                         fsTooltip = Nothing,
                         fsId = Nothing})
          Nothing)

currencyChoiceForm :: Html -> MForm Handler (FormResult NameAddress, Widget)
currencyChoiceForm = renderTable currencyChoiceAForm

getHomeR :: Handler Html
getHomeR = do
  (optionWidget, enctype) <- generateFormPost currencyChoiceForm
  defaultLayout $ do
    vals <- lookupSession "vals"
    holdings <- return $ extractHoldings (getNameAddresses vals)
    balances <- liftIO $ extractBalances holdings
    ticker <- liftIO getTicker
    holdingTuples <- return $ Data.List.zipWith (\h b -> (h, b)) holdings balances
    $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
  ((result, _), _) <- runFormPost currencyChoiceForm
  vals <- lookupSession "vals"
  case result of
    FormSuccess choice ->
      setSession "vals" $ toJson ([choice] <> (getNameAddresses vals))
    FormMissing ->
      redirect HomeR
    FormFailure _ ->
      redirect HomeR
  redirect HomeR

  
deleteHomeR :: Handler Html
deleteHomeR = do
  choice <- requireJsonBody :: Handler NameAddress
  vals <- lookupSession "vals"
  $(logInfo) $ pack $ show $ getNameAddresses vals
  setSession "vals" $
    toJson
    (Data.List.filter
      (\na -> ((toLower $ BS.name choice) /= (BS.name na)) || ((address choice /= (address na))))
      (getNameAddresses vals))
  redirect HomeR
