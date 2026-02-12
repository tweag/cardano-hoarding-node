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
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, addEvent, withSpan)
import Hoard.Effects.NodeToNode (ConnectToError (..), NodeToNode, connectToPeer)
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.PeerManager.Peers (Connection (..), awaitTermination, signalTermination)


collectFromPeer
    :: ( BlockRepo :> es
       , Conc :> es
       , Concurrent :> es
       , NodeToNode :> es
       , Pub :> es
       , Sub :> es
       , Tracing :> es
       )
    => Peer
    -> Connection
    -> Eff es (Maybe ConnectToError)
collectFromPeer peer conn = withSpan "collector.collect_from_peer" $ do
    addAttribute "peer.address" (show peer.address)
    addAttribute "peer.id" (show peer.id)
    addEvent "connecting" [("peer.address", show peer.address)]

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
    result <- tryReadMVar var

    case result of
        Nothing -> addEvent "disconnected" [("peer.address", show peer.address)]
        Just (ConnectToError errMsg) -> addEvent "connection_failed" [("peer.address", show peer.address), ("error", errMsg)]

    pure result


-- Filters events by peer ID and publishes block fetch requests for headers
-- that are not in the database.
pickBlockFetchRequest
    :: ( BlockRepo :> es
       , Pub :> es
       , Tracing :> es
       )
    => ID Peer
    -> HeaderReceived
    -> Eff es ()
pickBlockFetchRequest myPeerId event =
    unless (event.peer.id /= myPeerId) do
        let hash = blockHashFromHeader event.header
            slot = unSlotNo $ blockSlot event.header

        exists <- BlockRepo.blockExists hash

        unless exists $ withSpan "request_block_fetch" $ do
            addAttribute "slot" (show slot)
            addAttribute "hash" (show hash)
            addAttribute "peer.id" (show event.peer.id)
            publish
                BlockFetchRequest
                    { timestamp = event.timestamp
                    , header = event.header
                    , peer = event.peer
                    }
