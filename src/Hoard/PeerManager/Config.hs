module Hoard.PeerManager.Config
    ( Config (..)
    , PeerMode (..)
    ) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Data.Time (NominalDiffTime)

import Hoard.Types.QuietSnake (QuietSnake (..))


data PeerMode
    = -- | Connect to any eligible peer as well as any pinned peers.
      Automatic
    | -- | Only connect to pinned peers; ignore all other discovered peers.
      Manual
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake PeerMode


instance Default PeerMode where
    def = Automatic


data Config = Config
    { maxCollectorLifetimeSeconds :: NominalDiffTime
    , peerFailureCooldownSeconds :: NominalDiffTime
    , maxConcurrentCollectors :: Word
    , replenishIntervalSeconds :: Int
    , peerMode :: PeerMode
    , discoverNewPeers :: Bool
    -- ^ Whether to run the peer sharing protocol to discover new peers.
    -- When False, only peers already known from bootstrap or the API are used.
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { maxCollectorLifetimeSeconds = 60
            , peerFailureCooldownSeconds = 60
            , maxConcurrentCollectors = 100
            , replenishIntervalSeconds = 20
            , peerMode = def
            , discoverNewPeers = True
            }
