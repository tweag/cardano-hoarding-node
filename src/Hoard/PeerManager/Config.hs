module Hoard.PeerManager.Config
    ( Config (..)
    , PeerMode (..)
    , AutomaticConfig (..)
    ) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Data.Time (NominalDiffTime)

import Atelier.Time (Second)
import Atelier.Types.QuietSnake (QuietSnake (..))


data AutomaticConfig = AutomaticConfig
    { bootstrapPins :: Bool
    -- ^ When True, bootstrap peers are also added to selected_peers on
    -- first start (when selected_peers is empty), making them preferred
    -- during replenishment. Has no effect after the first start.
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake AutomaticConfig


instance Default AutomaticConfig where
    def = AutomaticConfig {bootstrapPins = False}


data PeerMode
    = -- | Connect to any eligible peer as well as any pinned peers.
      Automatic AutomaticConfig
    | -- | Only connect to pinned peers; ignore all other discovered peers.
      Manual
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake PeerMode


instance Default PeerMode where
    def = Automatic def


data Config = Config
    { maxCollectorLifetimeSeconds :: NominalDiffTime
    , peerFailureCooldownSeconds :: NominalDiffTime
    , maxConcurrentCollectors :: Word
    , replenishIntervalSeconds :: Second
    , peerMode :: PeerMode
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { maxCollectorLifetimeSeconds = 60
            , peerFailureCooldownSeconds = 60
            , maxConcurrentCollectors = 100
            , replenishIntervalSeconds = 20
            , peerMode = def
            }
