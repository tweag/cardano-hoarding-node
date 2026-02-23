module Hoard.PeerManager.Config
    ( Config (..)
    ) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Data.Time (NominalDiffTime)

import Hoard.Types.QuietSnake (QuietSnake (..))


data Config = Config
    { maxCollectorLifetimeSeconds :: NominalDiffTime
    , peerFailureCooldownSeconds :: NominalDiffTime
    , maxConcurrentCollectors :: Word
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { maxCollectorLifetimeSeconds = 60
            , peerFailureCooldownSeconds = 60
            , maxConcurrentCollectors = 100
            }
