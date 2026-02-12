module Hoard.PeerSharing.Config (Config (..)) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))

import Hoard.Types.QuietSnake (QuietSnake (..))


data Config = Config
    { requestIntervalMicroseconds :: Int
    -- ^ Interval between peer sharing requests
    , requestAmount :: Int
    -- ^ Number of peer addresses to request per query
    , maximumIngressQueue :: Int
    -- ^ Max bytes queued in ingress queue
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { requestIntervalMicroseconds = 3600_000_000 -- 1 hour
            , requestAmount = 100
            , maximumIngressQueue = 1000
            }
