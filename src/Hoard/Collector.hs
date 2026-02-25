module Hoard.Collector
    ( filterHeaderReceived
    , collectFromPeer
    ) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar (newEmptyMVar, putMVar, tryReadMVar)
import Prelude hiding (newEmptyMVar, putMVar, tryReadMVar)

import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, withSpan)
import Hoard.Effects.NodeToNode (ConnectToError (..), NodeToNode, connectToPeer)
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.Effects.Verifier (Verifier, getVerified, verifyCardanoHeader)
import Hoard.Events.ChainSync (HeaderReceived (..))
import Hoard.PeerManager.Peers (Connection (..), awaitTermination, signalTermination)

import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log qualified as Log
import Hoard.Events.BlockFetch qualified as BlockFetch


collectFromPeer
    :: ( BlockRepo :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , NodeToNode :> es
       , Pub BlockFetch.Request :> es
       , Sub HeaderReceived :> es
       , Tracing :> es
       , Verifier :> es
       )
    => Peer
    -> Connection
    -> Eff es (Maybe ConnectToError)
collectFromPeer peer conn = do
    Log.info $ "Collecting from " <> show peer.address

    var <- newEmptyMVar

    -- Use scoped to ensure listener and connection threads are cleaned up
    -- when the connection terminates (either normally or exceptionally)
    result <- Conc.scoped $ do
        _ <- Conc.fork_ do
            listen $ filterHeaderReceived peer.id

        _ <- Conc.fork do
            res <- connectToPeer peer
            -- If we reach this point, the connection closed unexpectedly, so we
            -- have to signal to the main collector thread that we have to shut
            -- down.
            putMVar var res
            signalTermination conn

        awaitTermination conn
        tryReadMVar var

    case result of
        Nothing -> do
            Log.info $ "Closing connection to " <> show peer.address
        Just (ConnectToError errMsg) -> do
            Log.warn $ "Error when collecting from " <> show peer.address <> ": " <> errMsg

    pure result


-- Filters events by peer ID and publishes block fetch requests for headers
-- that are not in the database.
filterHeaderReceived
    :: ( BlockRepo :> es
       , Pub BlockFetch.Request :> es
       , Tracing :> es
       , Verifier :> es
       )
    => ID Peer
    -> HeaderReceived
    -> Eff es ()
filterHeaderReceived myPeerId event =
    unless (event.peer.id /= myPeerId) $ withSpan "collector.filter_header_received" do
        let hash = blockHashFromHeader event.header
        addAttribute "peer.address" event.peer.address
        verifyCardanoHeader event.header >>= \case
            Left _invalidHeader -> do
                addAttribute "header.valid" False
            Right validHeader -> do
                addAttribute "header.valid" True
                let header = getVerified validHeader

                exists <- BlockRepo.blockExists hash

                if exists then do
                    addAttribute "duplicate_header" True
                else withSpan "request_block_fetch" do
                    addAttribute "duplicate_header" False
                    publish
                        BlockFetch.Request
                            { timestamp = event.timestamp
                            , peer = event.peer
                            , header
                            }
