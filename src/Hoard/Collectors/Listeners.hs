module Hoard.Collectors.Listeners
    ( dispatchDiscoveredNodes
    , collectorEvent
    , bracketCollector
    , pickBlockFetchRequest
    ) where

import Data.Set qualified as S
import Data.Time (diffUTCTime)
import Effectful (Eff, (:>))
import Effectful.Exception (ExitCase (..), generalBracket)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, modify, stateM)
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.Peer (Peer (..))
import Ouroboros.Consensus.Block
    ( SlotNo (..)
    , blockSlot
    )
import Prelude hiding (Reader, State, asks, modify, state)

import Hoard.BlockFetch.Events (BlockFetchRequest (..))
import Hoard.ChainSync.Events (HeaderReceived (..))
import Hoard.Collectors.Events (CollectorEvent (..))
import Hoard.Control.Exception (isGracefulShutdown, withExceptionLogging)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (PeerAddress (..))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode (NodeToNode, connectToPeer)
import Hoard.Effects.PeerRepo (PeerRepo, updatePeerFailure, upsertPeers)
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.PeerSharing.Events (PeersReceived (..))
import Hoard.Types.Environment (Config (..))
import Hoard.Types.HoardState (HoardState (..))


-- | Listener that logs collector events
collectorEvent :: (Log :> es) => CollectorEvent -> Eff es ()
collectorEvent = \case
    CollectorStarted addr ->
        Log.info $ "Collector: started for " <> show addr.host
    ConnectingToPeer addr ->
        Log.info $ "Collector: connecting to peer " <> show addr.host
    ConnectedToPeer addr ->
        Log.info $ "Collector: connected to peer " <> show addr.host
    ConnectionFailed addr reason ->
        Log.info $ "Collector: failed to connect to peer " <> show addr.host <> ": " <> reason
    ChainSyncReceived addr ->
        Log.info $ "Collector: chain sync received from " <> show addr.host
    BlockFetchReceived addr ->
        Log.info $ "Collector: block fetch received from " <> show addr.host


-- | Dispatch discovered peer nodes for connection.
--
-- When peers are discovered via peer sharing:
-- - Upserts all discovered peer addresses to the database
-- - Filters out peers already connected
-- - Filters out peers in cooldown period (recent failures)
-- - Starts collectors for all eligible peers
dispatchDiscoveredNodes
    :: ( BlockRepo :> es
       , Clock :> es
       , Conc :> es
       , Log :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , Reader Config :> es
       , State HoardState :> es
       , Sub :> es
       )
    => PeersReceived
    -> Eff es ()
dispatchDiscoveredNodes event = do
    Log.info "Dispatch: Received peers"

    -- First, upsert all discovered peer addresses to create Peer records
    timestamp <- Clock.currentTime
    upsertedPeers <- upsertPeers event.peerAddresses event.peer.address timestamp

    Log.info $ "Dispatch: " <> show (S.size upsertedPeers) <> " new peers to connect to"

    for_ upsertedPeers bracketCollector


-- | Fork a collector with exception handling and state management. The forked
-- collector verifies that the supplied peer is a valid peer to connect to.
--
-- This wraps runCollector with:
-- - Proper exception handling (forkTry + withExceptionLogging)
-- - Connected peers state management (adds peer on start, removes on termination)
-- - Failure time tracking (updates peer's lastFailureTime on exception)
--
-- The peer is skipped if:
-- - It is already connected to by another collector.
-- - If we have attempted to connect to the peer too recently.
--
-- The peer is automatically removed from connectedPeers when the collector
-- terminates, whether due to an exception or normal completion.
bracketCollector
    :: ( BlockRepo :> es
       , Clock :> es
       , Conc :> es
       , Log :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , Reader Config :> es
       , State HoardState :> es
       , Sub :> es
       )
    => Peer
    -> Eff es ()
bracketCollector peer = do
    _ <-
        Conc.forkTry @SomeException
            . withExceptionLogging ("collector " <> show peer.address)
            $ generalBracket
                ( stateM \r -> do
                    eligible <- isPeerEligible peer
                    if not (peer.id `S.member` r.connectedPeers) && eligible
                        then do
                            Log.debug $ "Adding peer to connectedPeers: " <> show peer.address
                            pure (Just peer, r {connectedPeers = S.insert peer.id r.connectedPeers})
                        else do
                            Log.debug $ "Peer is already connected to: " <> show peer.address
                            pure (Nothing, r)
                )
                ( \mp exitCase -> do
                    case mp of
                        Just p -> do
                            case exitCase of
                                -- Only update failure time for real errors, not clean shutdowns
                                ExitCaseException e ->
                                    unless (isGracefulShutdown e) do
                                        timestamp <- Clock.currentTime
                                        updatePeerFailure peer timestamp
                                _ -> pure ()

                            Log.debug $ "Removing peer from connectedPeers: " <> show peer.address
                            modify \r -> r {connectedPeers = S.delete p.id r.connectedPeers}

                            Log.info $ "ExitCase: " <> show exitCase
                        Nothing ->
                            Log.debug $ "Peer skipped: " <> show peer.address
                )
                (traverse_ collectFromPeer)
    pure ()


collectFromPeer
    :: ( BlockRepo :> es
       , Conc :> es
       , Log :> es
       , NodeToNode :> es
       , Pub :> es
       , Sub :> es
       )
    => Peer
    -> Eff es ()
collectFromPeer peer = do
    publish $ CollectorStarted peer.address
    publish $ ConnectingToPeer peer.address

    Conc.fork_ $ listen (pickBlockFetchRequest peer.id)
    _ <- connectToPeer peer

    publish $ ConnectedToPeer peer.address


-- | Check if a peer is eligible for connection based on failure cooldown
isPeerEligible :: (Reader Config :> es, Clock :> es) => Peer -> Eff es Bool
isPeerEligible peer = do
    currentTime <- Clock.currentTime
    cooldown <- asks (.peerFailureCooldown)
    case peer.lastFailureTime of
        Nothing -> pure True -- Never failed, eligible
        Just failureTime ->
            let timeSinceFailure = diffUTCTime currentTime failureTime
            in  pure $ timeSinceFailure >= cooldown


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
