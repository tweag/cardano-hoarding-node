module Hoard.Types.HoardState
  ( HoardState (..),
  )
where

import Data.Default (Default (..))

-- | Application state
data HoardState = HoardState {}
  deriving (Show, Eq)

instance Default HoardState where
  def = HoardState {}
