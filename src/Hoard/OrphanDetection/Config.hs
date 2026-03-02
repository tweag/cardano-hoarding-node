module Hoard.OrphanDetection.Config
    ( Config (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))

import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Configuration for orphan detection
data Config = Config
    { agingBatchSize :: !Int
    -- ^ Maximum number of unclassified blocks to age per immutable tip update.
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { agingBatchSize = 1000
            }
