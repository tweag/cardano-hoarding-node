module Hoard.KeepAlive.Config (Config (..)) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))

import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Keep-alive protocol configuration
data Config = Config
    { intervalMicroseconds :: Int
    -- ^ Interval between keepalive messages
    , maximumIngressQueue :: Int
    -- ^ Max bytes queued in ingress queue
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { intervalMicroseconds = 10_000_000
            , maximumIngressQueue = 1000
            }
