{-# LANGUAGE OverloadedStrings #-}
module Handler.RefreshSession where

import Import
import Common.Util

postRefreshSessionR :: Handler Text
postRefreshSessionR = do
  vals <- lookupSession "vals"
  setSession "vals" $ toJson (getNameAddresses vals)
  return ""

