module Hoard.Collectors.Listeners
    ( dispatchDiscoveredNodes
    , collectorEvent
    , bracketCollector
    ) where

import Data.Set qualified as S
import Data.Time (diffUTCTime)
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Exception (ExitCase (..), generalBracket)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, modify, state, stateM)
import Effectful.Timeout (Timeout)
import Prelude hiding (Reader, State, asks, modify, state)

import Hoard.BlockFetch.Events (BlockFetchRequest (..), BlockReceived (..))
import Hoard.ChainSync.Events (HeaderReceived (..))
import Hoard.Collectors.Events (CollectorEvent (..))
import Hoard.Collectors.State (BlocksBeingFetched (..))
import Hoard.Control.Exception (isGracefulShutdown, withExceptionLogging)
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Input (Input, input, runInputChan, runTimedBatchedInput)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode (NodeToNode, connectToPeer)
import Hoard.Effects.NodeToNode qualified as NodeToNode
import Hoard.Effects.Output (Output, output, runOutputChan)
import Hoard.Effects.PeerRepo (PeerRepo, updatePeerFailure, upsertPeers)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Network.Events (PeersReceived (..))
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
       , Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , Reader Config :> es
       , State BlocksBeingFetched :> es
       , State HoardState :> es
       , Timeout :> es
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
       , Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , Reader Config :> es
       , State BlocksBeingFetched :> es
       , State HoardState :> es
       , Timeout :> es
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
       , Chan :> es
       , Conc :> es
       , Concurrent :> es
       , NodeToNode :> es
       , Pub :> es
       , State BlocksBeingFetched :> es
       , Timeout :> es
       )
    => Peer
    -> Eff es Void
collectFromPeer peer =
    withBridge @BlockFetchRequest
        . withBridge @HeaderReceived
        . withBridge @BlockReceived
        . runTimedBatchedInput @BlockFetchRequest 10 1_000_000
        $ do
            publish $ CollectorStarted peer.address
            publish $ ConnectingToPeer peer.address

            Conc.fork_ pickBlockFetchRequest
            _conn <-
                connectToPeer $
                    NodeToNode.Config
                        { peer
                        , emitFetchedHeader = output
                        , emitFetchedBlock = output
                        , awaitBlockFetchRequests = input
                        }
            publish $ ConnectedToPeer peer.address

            -- Connection is now running autonomously!
            -- Protocols publish events as they receive data
            -- For now, just keep the collector alive
            forever $ threadDelay 1000000


-- | Re-emit `HeaderReceived` events as `BlockFetchRequests`.
pickBlockFetchRequest
    :: ( BlockRepo :> es
       , Input HeaderReceived :> es
       , State BlocksBeingFetched :> es
       , Output BlockFetchRequest :> es
       )
    => Eff es Void
pickBlockFetchRequest = forever do
    event <- input
    let hash = blockHashFromHeader event.header
    -- This check can be implemented in 2 thread-safe ways, as far as we know:
    -- 1. Keep the database check inside the `stateM` operation and ensure
    --    thread-safety between checking the DB and the `blocksBeingFetched`
    --    state.
    -- 2. Check the in-memory `blocksBeingFetched` state first, update
    --    `blocksBeingFetched` if the hash is not in there, then check the
    --    database, removing the hash from `blocksBeingFetched` if it is
    --    already in the database.
    --
    -- We decided to use option 2 to prevent having the database check inside
    -- `state`/`stateM`, which would drastically increase the time spent in the
    -- critical section `state`/`stateM` provides us.
    fetchTheBlock <- state \s ->
        if hash `S.notMember` s.blocksBeingFetched
            then
                (True, coerce S.insert hash s)
            else
                (False, s)
    when fetchTheBlock do
        existingBlock <- BlockRepo.getBlock event.header
        if (isJust existingBlock)
            then
                output
                    BlockFetchRequest
                        { timestamp = event.timestamp
                        , header = event.header
                        , peer = event.peer
                        }
            else
                modify $ coerce S.delete hash


-- | Bridge effects through bidirectional channels.
--
-- Creates a channel pair and interprets Input/Output effects over them,
-- enabling communication between threads.
--
-- This function:
-- 1. Creates a bidirectional channel pair using 'Chan.newChan'
-- 2. Interprets 'Input' and 'Output' effects over these channels
-- 3. Provides these effects to the wrapped action
-- 4. The action can fork threads that communicate via these effects
withBridge :: forall b es a. (Chan :> es) => Eff (Input b : Output b : es) a -> Eff es a
withBridge action = do
    (inChan, outChan) <- Chan.newChan
    runOutputChan inChan . runInputChan outChan $ action


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
