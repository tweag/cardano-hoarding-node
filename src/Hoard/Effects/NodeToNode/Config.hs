module Hoard.Effects.NodeToNode.Config (Config (..)) where

import Hoard.ChainSync.Events (HeaderReceived (..))
import Hoard.Data.Peer (Peer (..))
import Hoard.Network.Events (BlockFetchRequest (..), BlockReceived (..))


data Config m = Config
    { awaitBlockFetchRequests :: m (NonEmpty BlockFetchRequest)
    , emitFetchedHeader :: HeaderReceived -> m ()
    , emitFetchedBlock :: BlockReceived -> m ()
    , peer :: Peer
    }
