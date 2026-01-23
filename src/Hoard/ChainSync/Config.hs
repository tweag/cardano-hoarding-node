module Hoard.ChainSync.Config (Config (..)) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Hoard.Types.QuietSnake (QuietSnake (..))


data Config = Config
    { maximumIngressQueue :: Int
    -- ^ Max bytes queued in ingress queue
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { maximumIngressQueue = 1200
            }
