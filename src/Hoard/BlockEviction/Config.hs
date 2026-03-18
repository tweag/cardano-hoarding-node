module Hoard.BlockEviction.Config
    ( Config (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))

import Atelier.Types.QuietSnake (QuietSnake (..))


-- | Configuration for block eviction
data Config = Config
    { evictionIntervalSeconds :: !Int
    -- ^ How often the eviction trigger runs (in seconds).
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { evictionIntervalSeconds = 3600 -- 1 hour
            }
