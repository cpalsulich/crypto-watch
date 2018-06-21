{-# LANGUAGE OverloadedStrings #-}
module Binding.SessionHolding where

import Data.Text

data NameAddress = NameAddress
  { name :: Text,
    address :: Text
  }
