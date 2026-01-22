module Hoard.Collectors.State (BlocksBeingFetched (..), ConnectedPeers (..)) where

import Data.Default (Default (..))

import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)


newtype BlocksBeingFetched = BlocksBeingFetched
    { blocksBeingFetched :: (Set BlockHash)
    }
    deriving stock (Eq, Show)


instance Default BlocksBeingFetched where
    def = BlocksBeingFetched mempty


newtype ConnectedPeers = ConnectedPeers
    { connectedPeers :: Set (ID Peer)
    }


instance Default ConnectedPeers where
    def = ConnectedPeers mempty
