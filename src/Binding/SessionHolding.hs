{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Binding.SessionHolding where

import Data.Aeson
import Data.Text
import GHC.Generics

data NameAddress = NameAddress
  { name :: Text,
    address :: Text }
  deriving (Generic, Show)

instance ToJSON NameAddress where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON NameAddress
