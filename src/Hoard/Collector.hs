module Hoard.Collector
    ( pickBlockFetchRequest
    , collectFromPeer
    ) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar (newEmptyMVar, putMVar, tryReadMVar)
import Ouroboros.Consensus.Block (SlotNo (..), blockSlot)
import Prelude hiding (Reader, State, asks, get, modify, newEmptyMVar, putMVar, state, takeMVar, tryReadMVar)

import Hoard.BlockFetch.Events (BlockFetchRequest (..))
import Hoard.ChainSync.Events (HeaderReceived (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode (ConnectToError, NodeToNode, connectToPeer)
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.PeerManager.Peers (Connection (..), awaitTermination, signalTermination)


collectFromPeer
    :: ( BlockRepo :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , NodeToNode :> es
       , Pub :> es
       , Sub :> es
       )
    => Peer
    -> Connection
    -> Eff es (Maybe ConnectToError)
collectFromPeer peer conn = do
    Conc.fork_ $ listen (pickBlockFetchRequest peer.id)

    var <- newEmptyMVar
    _ <- Conc.fork do
        res <- connectToPeer peer
        -- If we reach this point, the connection closed unexpectedly, so we
        -- have to signal to the main collector thread that we have to shut
        -- down.
        putMVar var res
        signalTermination conn

    awaitTermination conn
    tryReadMVar var


-- Filters events by peer ID and publishes block fetch requests for headers
-- that are not in the database.
pickBlockFetchRequest
    :: ( BlockRepo :> es
       , Pub :> es
       , Log :> es
       )
    => ID Peer
    -> HeaderReceived
    -> Eff es ()
pickBlockFetchRequest myPeerId event =
    unless (event.peer.id /= myPeerId) do
        let hash = blockHashFromHeader event.header
            slot = unSlotNo $ blockSlot event.header

        existingBlock <- BlockRepo.getBlock event.header

        when (isNothing existingBlock) $ do
            Log.info $ "Publishing block fetch request for slot " <> show slot <> " (hash: " <> show hash <> ")"
            publish
                BlockFetchRequest
                    { timestamp = event.timestamp
                    , header = event.header
                    , peer = event.peer
                    }
