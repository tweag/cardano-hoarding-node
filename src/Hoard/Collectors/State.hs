module Hoard.Collectors.State (BlocksBeingFetched (..)) where

import Data.Default (Default (..))
import Data.Set qualified as Set

import Hoard.Data.BlockHash (BlockHash)


newtype BlocksBeingFetched = BlocksBeingFetched {blocksBeingFetched :: (Set BlockHash)}
    deriving stock (Eq, Show)


instance Default BlocksBeingFetched where
    def = BlocksBeingFetched Set.empty
