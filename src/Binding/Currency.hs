module Binding.Currency where

import Data.Text

class Holding a where
  getName :: a -> Text
  getBalance :: a -> Address -> IO (Maybe Text)
  getAddress :: a -> Address

type Address = Text
