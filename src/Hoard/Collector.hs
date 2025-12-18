module Hoard.Collector (runCollector, runCollectors) where

import Control.Concurrent (threadDelay)
import Data.Set qualified as S
import Data.Set qualified as Set
import Effectful (Eff, IOE, (:>))
import Effectful.Exception (ExitCase (..), generalBracket)
import Effectful.State.Static.Shared (State, modify, state)
import Prelude hiding (State, gets, modify, state)

import Hoard.Bootstrap (bootstrapPeer)
import Hoard.Control.Exception (isGracefulShutdown, withExceptionLogging)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Input (Input, input, runInputChan)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode (Config (..), NodeToNode, connectToPeer)
import Hoard.Effects.NodeToNode qualified as NodeToNode
import Hoard.Effects.Output (Output, output, runOutputChan)
import Hoard.Effects.PeerRepo (PeerRepo, getAllPeers, updatePeerFailure)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Events.Collector (CollectorEvent (..))
import Hoard.Network.Events
    ( BlockFetchRequest (..)
    , BlockReceivedData (..)
    , HeaderReceivedData (..)
    )
import Hoard.Types.HoardState (HoardState, connectedPeers)


-- | Start collectors for all known peers, or bootstrap if none exist.
--
-- This queries the database for known peers and:
-- - If peers exist, starts collectors for all of them
-- - If no peers exist, bootstraps from the hardcoded preview-node peer
runCollectors
    :: ( Pub :> es
       , NodeToNode :> es
       , Chan :> es
       , Conc :> es
       , PeerRepo :> es
       , Clock :> es
       , Log :> es
       , State HoardState :> es
       , IOE :> es
       )
    => Eff es ()
runCollectors = do
    -- Check if there are any known peers in the database
    knownPeers <- getAllPeers

    if Set.null knownPeers
        then do
            -- No known peers, bootstrap from hardcoded peer
            Log.debug "No known peers found, bootstrapping from hardcoded peer"
            peer <- bootstrapPeer
            runCollector peer
        else do
            -- Start collectors for all known peers
            let peerCount = Set.size knownPeers
            Log.debug $ "Found " <> show peerCount <> " known peers, starting collectors"
            forM_ (Set.toList knownPeers) runCollector


-- | Fork a collector with exception handling and state management.
--
-- This wraps runCollector with:
-- - Proper exception handling (forkTry + withExceptionLogging)
-- - Connected peers state management (adds peer on start, removes on termination)
-- - Failure time tracking (updates peer's lastFailureTime on exception)
--
-- The peer is automatically removed from connectedPeers when the collector
-- terminates, whether due to an exception or normal completion.
runCollector
    :: ( Conc :> es
       , Chan :> es
       , Clock :> es
       , IOE :> es
       , Log :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , State HoardState :> es
       )
    => Peer
    -> Eff es ()
runCollector peer = do
    _ <-
        Conc.forkTry @SomeException
            . withExceptionLogging ("collector " <> show peer.address)
            $ generalBracket
                (state \r -> (peer, r {connectedPeers = S.insert peer r.connectedPeers}))
                ( \p exitCase -> do
                    case exitCase of
                        -- Only update failure time for real errors, not clean shutdowns
                        ExitCaseException e ->
                            unless (isGracefulShutdown e) $ do
                                timestamp <- Clock.currentTime
                                updatePeerFailure peer timestamp
                        _ -> pure ()

                    modify \r -> r {connectedPeers = S.delete p r.connectedPeers}
                )
                runCollectorImpl
    pure ()


runCollectorImpl
    :: ( Conc :> es
       , Chan :> es
       , IOE :> es
       , NodeToNode :> es
       , Pub :> es
       )
    => Peer
    -> Eff es Void
runCollectorImpl peer =
    withBridge @BlockFetchRequest
        . withBridge @HeaderReceivedData
        . withBridge @BlockReceivedData
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
                        , awaitBlockFetchRequest = input
                        }
            publish $ ConnectedToPeer peer.address

            -- Connection is now running autonomously!
            -- Protocols publish events as they receive data
            -- For now, just keep the collector alive
            forever $ liftIO $ threadDelay 1000000


-- | Re-emit `HeaderReceived` events as `BlockFetchRequests`.
pickBlockFetchRequest
    :: ( Input HeaderReceivedData :> es
       , Output BlockFetchRequest :> es
       )
    => Eff es Void
pickBlockFetchRequest = forever do
    dat <- input
    output $
        BlockFetchRequest
            { timestamp = dat.timestamp
            , point = dat.point
            , peer = dat.peer.address
            }


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
