module Main (main) where

import Data.Default (def)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (evalState)
import Effectful.Temporary (runTemporary)
import Effectful.Timeout (runTimeout)
import Prelude hiding (evalState)

import Hoard.BlockFetch qualified as BlockFetch
import Hoard.BlockFetch.Events
    ( BlockBatchCompleted
    , BlockFetchFailed
    , BlockFetchRequest
    , BlockFetchStarted
    , BlockReceived
    )
import Hoard.Bootstrap (bootstrapPeers)
import Hoard.ChainSync qualified as ChainSync
import Hoard.ChainSync.Events (ChainSyncIntersectionFound, ChainSyncStarted, HeaderReceived (..), RollBackward, RollForward)
import Hoard.Control.Exception (runErrorThrowing)
import Hoard.Effects.BlockRepo (runBlockRepo)
import Hoard.Effects.Chan (runChan)
import Hoard.Effects.Clock (runClock)
import Hoard.Effects.Conc (runConcNewScope)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.DBRead (runDBRead)
import Hoard.Effects.DBWrite (runDBWrite)
import Hoard.Effects.Environment (loadEnv, runConfigReader, runHandlesReader)
import Hoard.Effects.HeaderRepo (runHeaderRepo)
import Hoard.Effects.HoardStateRepo (runHoardStateRepo)
import Hoard.Effects.Log (runLog)
import Hoard.Effects.Monitoring.Metrics (runMetrics)
import Hoard.Effects.Monitoring.Tracing (runTracingFromConfig)
import Hoard.Effects.NodeToClient (runNodeToClient)
import Hoard.Effects.NodeToNode (runNodeToNode)
import Hoard.Effects.Options (loadOptions)
import Hoard.Effects.PeerRepo (runPeerRepo)
import Hoard.Effects.Publishing (runPubSub)
import Hoard.Effects.WithSocket (withNodeSockets)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered)
import Hoard.KeepAlive.NodeToNode (KeepAlivePing)
import Hoard.Listeners (runListeners)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed)
import Hoard.Monitoring (Poll)
import Hoard.Monitoring qualified as Monitoring
import Hoard.Network.Events (ProtocolError)
import Hoard.OrphanDetection qualified as OrphanDetection
import Hoard.PeerManager (CullRequested, PeerDisconnected, PeerRequested)
import Hoard.PeerManager qualified as PeerManager
import Hoard.PeerManager.Peers (Peers)
import Hoard.PeerSharing qualified as PeerSharing
import Hoard.PeerSharing.Events (PeerSharingFailed, PeerSharingStarted, PeersReceived)
import Hoard.Server (runServer)
import Hoard.Setup (setup)
import Hoard.Triggers (runTriggers)
import Hoard.Types.HoardState (HoardState)


main :: IO ()
main =
    runEff
        . runConcurrent
        . runTimeout
        . runChan
        . runConcNewScope
        . loadOptions
        . loadEnv
        . runConfigReader
        . runHandlesReader
        . runLog
        . runClock
        . runFileSystem
        . runTemporary
        . runMetrics
        . withNodeSockets
        . runErrorThrowing
        . evalState @HoardState def
        . evalState @Peers def
        . runTracingFromConfig
        -- Pub/Sub handlers for all event types
        . runPubSub @ChainSyncStarted
        . runPubSub @ChainSyncIntersectionFound
        . runPubSub @RollBackward
        . runPubSub @RollForward
        . runPubSub @HeaderReceived
        . runPubSub @BlockFetchStarted
        . runPubSub @BlockFetchRequest
        . runPubSub @BlockReceived
        . runPubSub @BlockBatchCompleted
        . runPubSub @BlockFetchFailed
        . runPubSub @PeerSharingStarted
        . runPubSub @PeersReceived
        . runPubSub @PeerSharingFailed
        . runPubSub @CullRequested
        . runPubSub @PeerRequested
        . runPubSub @PeerDisconnected
        . runPubSub @Poll
        . runPubSub @ImmutableTipRefreshTriggered
        . runPubSub @ImmutableTipRefreshed
        . runPubSub @ProtocolError
        . runPubSub @KeepAlivePing
        . runNodeToClient
        . runNodeToNode
        . runDBRead
        . runDBWrite
        . runHeaderRepo
        . runPeerRepo
        . runBlockRepo
        . runHoardStateRepo
        $ do
            setup
            void bootstrapPeers
            runServer
            runListeners
            runTriggers
            PeerSharing.run
            ChainSync.run
            BlockFetch.run
            OrphanDetection.run
            Monitoring.run
            PeerManager.run
            Conc.awaitAll
