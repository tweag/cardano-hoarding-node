module Hoard.Collector
    ( pickBlockFetchRequest
    , collectFromPeer
    ) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar (newEmptyMVar, putMVar, tryReadMVar)
import Ouroboros.Consensus.Block (SlotNo (..), blockSlot)
import Prelude hiding (Reader, State, asks, get, modify, newEmptyMVar, putMVar, state, takeMVar, tryReadMVar)

import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Monitoring.Tracing (Attr (..), Tracing, addAttribute, addEvent, withSpan)
import Hoard.Effects.NodeToNode (ConnectToError (..), NodeToNode, connectToPeer)
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.Effects.Verifier (Verifier, getVerified, verifyCardanoHeader)
import Hoard.Events.ChainSync (HeaderReceived (..))
import Hoard.PeerManager.Peers (Connection (..), awaitTermination, signalTermination)

import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.Conc qualified as Conc
import Hoard.Events.BlockFetch qualified as BlockFetch


collectFromPeer
    :: ( BlockRepo :> es
       , Conc :> es
       , Concurrent :> es
       , NodeToNode :> es
       , Pub BlockFetch.Request :> es
       , Sub HeaderReceived :> es
       , Tracing :> es
       , Verifier :> es
       )
    => Peer
    -> Connection
    -> Eff es (Maybe ConnectToError)
collectFromPeer peer conn = withSpan "collector.collect_from_peer" $ do
    addAttribute "peer.address" peer.address
    addAttribute "peer.id" peer.id
    addEvent "connecting" [("peer.address", peer.address)]

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
        Nothing -> addEvent "disconnected" [("peer.address", peer.address)]
        Just (ConnectToError errMsg) ->
            addEvent
                "connection_failed"
                [ ("peer.address", Attr peer.address)
                , ("error", Attr errMsg)
                ]

    pure result


-- Filters events by peer ID and publishes block fetch requests for headers
-- that are not in the database.
pickBlockFetchRequest
    :: ( BlockRepo :> es
       , Pub BlockFetch.Request :> es
       , Tracing :> es
       , Verifier :> es
       )
    => ID Peer
    -> HeaderReceived
    -> Eff es ()
pickBlockFetchRequest myPeerId event =
    unless (event.peer.id /= myPeerId)
        $ verifyCardanoHeader event.header >>= \case
            Left _invalidHeader -> do
                addEvent "collector_invalid_header" [("hash", blockHashFromHeader event.header)]
            Right validHeader -> do
                let header = getVerified validHeader
                    hash = blockHashFromHeader header
                    slot = unSlotNo $ blockSlot header

                exists <- BlockRepo.blockExists hash

                unless exists $ withSpan "request_block_fetch" $ do
                    addAttribute "slot" $ fromIntegral @_ @Int64 slot
                    addAttribute "hash" hash
                    addAttribute "peer.id" event.peer.id
                    publish
                        BlockFetch.Request
                            { timestamp = event.timestamp
                            , peer = event.peer
                            , header
                            }
