module Hoard.Effects.NodeToNode.Config (Config (..)) where

import Hoard.BlockFetch.Events (BlockFetchRequest (..), BlockReceived (..))
import Hoard.ChainSync.Events (HeaderReceived (..))
import Hoard.Data.Peer (Peer (..))


data Config m = Config
    { awaitBlockFetchRequests :: m (NonEmpty BlockFetchRequest)
    , emitFetchedHeader :: HeaderReceived -> m ()
    , emitFetchedBlock :: BlockReceived -> m ()
    , peer :: Peer
    }
