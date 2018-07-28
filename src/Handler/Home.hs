{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Prelude as P

import Data.Aeson
import Data.List
import Data.Map.Internal
import Data.Time.Clock.POSIX
import Import
import Yesod.Form.Bootstrap4

import qualified Data.ByteString.Lazy as DBL

import Binding.Currency
import Binding.SessionHolding as BS

import Binding.Ark (getArkHolding)
import Binding.Btc (getBtcHolding)
import Binding.Eth (getEthHolding)
import Binding.Xrp (getXrpHolding)
import External.Coinmarketcap as EC

type NameAddresses = [NameAddress]

currency :: Text
currency = "USD"

fromJson :: FromJSON a => Text -> Maybe a
fromJson = (decode P.. DBL.fromStrict P.. encodeUtf8)

toJson :: ToJSON a => a -> Text
toJson = (decodeUtf8 P.. DBL.toStrict P.. encode)

getTickerBalance :: Maybe Ticker -> Text -> Maybe Float -> Maybe Float
getTickerBalance mTicker val mAmount = mTicker
  >>= (\x -> Just $ elems x)
  >>= (\currencies -> Data.List.find (\x -> (toLower val) == (pack P.. toLower P.. EC.symbol $ x)) currencies)
  >>= (Data.Map.Internal.lookup $ unpack currency) P.. quotes
  >>= (\x -> maybe Nothing (\amount -> Just $ (price x) * amount) mAmount)

getNameAddresses :: Maybe Text -> [NameAddress]
getNameAddresses Nothing = []
getNameAddresses (Just jsonText) = maybe [] P.id $ fromJson jsonText

getHoldingName :: IHolding -> Text
getHoldingName (MkHolding a) = getName a

getHoldingAddress :: IHolding -> Text
getHoldingAddress (MkHolding a) = getAddress a

getHoldingBalance :: IHolding -> IO (Maybe Float)
getHoldingBalance (MkHolding a) = getBalance a

optionMap :: Map Text (Text -> IHolding)
optionMap = Data.Map.Internal.fromList [("ark", (\addr -> packHolding $ getArkHolding addr)),
                                        ("eth", (\addr -> packHolding $ getEthHolding addr)),
                                        ("btc", (\addr -> packHolding $ getBtcHolding addr)),
                                        ("xrp", (\addr -> packHolding $ getXrpHolding addr))]

getHolding :: NameAddress -> IHolding
getHolding na = optionMap ! (BS.name na) $ BS.address na

extractHoldings :: [NameAddress] -> [IHolding]
extractHoldings nas = (fmap) getHolding nas

extractBalances :: [IHolding] -> IO ([Maybe Float])
extractBalances hs = sequence $ (fmap) (\h -> getHoldingBalance h) hs

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
--    where
--      extractTimestamp :: Maybe Text -> Integer
--      extractTimestamp val = (read P.. unpack P.. maybe "0" P.id) val
--      getTickerData :: Maybe Text -> Integer -> Integer  -> HandlerFor App (Maybe Ticker)
--      getTickerData tickerJson sessionTimestamp currentTimestamp =
--        if 300 < currentTimestamp - sessionTimestamp
--          then do updateTickerSessions currentTimestamp >> return $ maybe Nothing fromJson tickerJson
--          else do return $ maybe Nothing fromJson tickerJson
--      updateTickerSessions :: Integer -> HandlerFor App ()
--      updateTickerSessions currentTimestamp = do
--        ticker <- 
--        setSession "ticker" $ toJson ticker
--        setSession "tickerTimestamp" $ pack $ show $ currentTimestamp
--    sessionTimestamp <- extractTimestamp <$> lookupSession "tickerTimestamp"
--    currentTimestamp <- liftIO $ (toInteger P.. round) <$> getPOSIXTime
    
--    ticker <- getTickerData

postHomeR :: Handler Html
postHomeR = do
  ((result, _), _) <- runFormPost currencyChoiceForm
  vals <- lookupSession "vals"
  case result of
    FormSuccess choice ->
      setSession "vals" $ (decodeUtf8 P.. DBL.toStrict P.. encode) ([choice] <> (getNameAddresses vals))
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
    (decodeUtf8 P.. DBL.toStrict P.. encode)
    (Data.List.filter
      (\na -> ((toLower $ BS.name choice) /= (BS.name na)) || ((address choice /= (address na))))
      (getNameAddresses vals))
  redirect HomeR
