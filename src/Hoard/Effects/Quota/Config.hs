module Hoard.Effects.Quota.Config
    ( Config (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import Data.Time (NominalDiffTime)

import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Configuration for quota tracking
data Config = Config
    { entryTtl :: !NominalDiffTime
    -- ^ Time-to-live for quota entries. After this duration, entries are evicted from the cache.
    , cleanupInterval :: !NominalDiffTime
    -- ^ How often the background cleanup thread runs to evict expired entries.
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { entryTtl = 12 * 3600 -- 12 hours in seconds
            , cleanupInterval = 3600 -- 1 hour in seconds
            }
