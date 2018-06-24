{-# LANGUAGE ExistentialQuantification #-}
module Binding.Currency where

import Data.Text

data IHolding = forall a . Holding a => MkHolding a

class Holding a where
  getName :: a -> Text
  getBalance :: a -> IO (Maybe Float)
  getAddress :: a -> Text

packHolding :: Holding a => a -> IHolding
packHolding = MkHolding
