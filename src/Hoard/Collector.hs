module Hoard.Collector
    ( runCollector
    , runCollectors
    , bracketCollector
    ) where

import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Time (diffUTCTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Exception (ExitCase (..), generalBracket)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, modify, stateM)
import Effectful.Timeout (Timeout)
import Ouroboros.Consensus.Block.Abstract (headerPoint)
import Prelude hiding (Reader, State, asks, gets, modify, state)

import Hoard.Bootstrap (bootstrapPeers)
import Hoard.Control.Exception (isGracefulShutdown, withExceptionLogging)
import Hoard.Data.Peer (Peer (..))
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
import Hoard.Effects.PeerRepo (PeerRepo, getAllPeers, updatePeerFailure)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Events.Collector (CollectorEvent (..))
import Hoard.Network.Events
    ( BlockFetchRequest (..)
    , BlockReceived (..)
    , HeaderReceived (..)
    )
import Hoard.Types.Environment (Config (..))
import Hoard.Types.HoardState (HoardState, connectedPeers)


-- | Start collectors for all known peers, or bootstrap if none exist.
--
-- This queries the database for known peers and:
-- - If peers exist, starts collectors for all of them
-- - If no peers exist, bootstraps from the hardcoded preview-node peer
runCollectors
    :: ( BlockRepo :> es
       , Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Log :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , Reader Config :> es
       , State HoardState :> es
       , Timeout :> es
       )
    => Eff es ()
runCollectors = do
    -- Check if there are any known peers in the database
    knownPeers <- getAllPeers

    if Set.null knownPeers
        then do
            -- No known peers, bootstrap from peer snapshot
            Log.debug "No known peers found, bootstrapping from peer snapshot"
            bootstrappedPeers <- bootstrapPeers
            let peerCount = Set.size bootstrappedPeers
            Log.debug $ "Bootstrapped " <> show peerCount <> " peers from peer snapshot"
            for_ (Set.toList bootstrappedPeers) bracketCollector
        else do
            -- Start collectors for all known peers
            let peerCount = Set.size knownPeers
            Log.debug $ "Found " <> show peerCount <> " known peers, starting collectors"
            for_ (Set.toList knownPeers) bracketCollector


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
       , Concurrent :> es
       , Conc :> es
       , Chan :> es
       , Clock :> es
       , Log :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , Reader Config :> es
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
                (traverse_ runCollector)
    pure ()


runCollector
    :: ( BlockRepo :> es
       , Conc :> es
       , Concurrent :> es
       , Chan :> es
       , Log :> es
       , NodeToNode :> es
       , Pub :> es
       , Timeout :> es
       )
    => Peer
    -> Eff es Void
runCollector peer =
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
       , Log :> es
       , Output BlockFetchRequest :> es
       )
    => Eff es Void
pickBlockFetchRequest = forever do
    event <- input
    let headers = [event.header]
    existingBlocks <- BlockRepo.hasBlocks headers
    if length existingBlocks < length headers
        then do
            output $
                BlockFetchRequest
                    { timestamp = event.timestamp
                    , point = headerPoint event.header
                    , peer = event.peer
                    }
        else
            Log.info "BlockFetch: already have block of found header. Skipping..."


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
