module Hoard.Effects.Quota.Config
    ( Config (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))

import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Configuration for quota tracking
data Config = Config
    { maxBlocksPerPeerPerSlot :: !Int
    -- ^ Maximum number of blocks allowed per (peer, slot) pair
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def = Config {maxBlocksPerPeerPerSlot = 1}
