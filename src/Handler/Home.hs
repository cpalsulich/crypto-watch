{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
--import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Binding.Ark (getArkHolding)
import Binding.Currency

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    arkHolding <- return $ getArkHolding $ pack "AGDENwv5qXV2zfXQBh9nUdNesdX8Yyk7ow"
    arkBalance <- liftIO
      $ getBalance arkHolding (getAddress arkHolding)
    $(widgetFile "homepage")
