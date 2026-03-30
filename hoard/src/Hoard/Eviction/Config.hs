module Hoard.Eviction.Config
    ( Config (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))

import Atelier.Time (Second)
import Atelier.Types.QuietSnake (QuietSnake (..))


data Config = Config
    { evictionIntervalSeconds :: !Second
    -- ^ How often the eviction trigger runs (in seconds).
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { evictionIntervalSeconds = 3600 -- 1 hour
            }
