module Hoard.Data.Header
    ( Header (..)
    )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time (UTCTime)

import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)
import Prelude hiding (id)


-- | Represents a block header received from the Cardano network
--
-- This is a minimal schema for step 1. Additional header data fields
-- (slot, hash, block number, etc.) will be added in step 2.
data Header = Header
    { id :: ID Header
    , receivedAt :: UTCTime
    , receivedFromPeerId :: ID Peer
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)
