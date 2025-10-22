module Hoard.Data.Peer
    ( Peer (..)
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Hoard.Data.ID (ID)


-- | Represents a peer in the P2P network
data Peer = Peer
    { id :: ID Peer
    , address :: Text
    , port :: Int
    , firstDiscovered :: UTCTime
    , lastSeen :: UTCTime
    , discoveredVia :: Text
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
