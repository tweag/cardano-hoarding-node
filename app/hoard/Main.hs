module Main (main) where

import Data.Default (def)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (evalState)
import Effectful.Temporary (runTemporary)
import Effectful.Timeout (runTimeout)
import Prelude hiding (evalState)

import Hoard.BlockFetch (BlockFetch (..))
import Hoard.BlockFetch.Events
    ( BlockBatchCompleted
    , BlockFetchFailed
    , BlockFetchRequest
    , BlockFetchStarted
    , BlockReceived
    )
import Hoard.ChainSync (ChainSync (..))
import Hoard.ChainSync.Events (ChainSyncIntersectionFound, ChainSyncStarted, HeaderReceived (..), RollBackward, RollForward)
import Hoard.Component (component, runSystem)
import Hoard.Control.Exception (runErrorThrowing)
import Hoard.Core (Core (..))
import Hoard.Effects.BlockRepo (runBlockRepo)
import Hoard.Effects.Chan (runChan)
import Hoard.Effects.Clock (runClock)
import Hoard.Effects.Conc (runConcNewScope)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.ConfigPath (runConfigPath)
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
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed)
import Hoard.Monitoring (Monitoring (..), Poll)
import Hoard.Monitoring qualified as Monitoring
import Hoard.Network.Events (ProtocolError)
import Hoard.OrphanDetection (OrphanDetection (..))
import Hoard.PeerManager (CullRequested, PeerDisconnected, PeerManager (..), PeerRequested)
import Hoard.PeerManager.Peers (Peers)
import Hoard.PeerSharing (PeerSharing (..))
import Hoard.PeerSharing.Events (PeerSharingFailed, PeerSharingStarted, PeersReceived)
import Hoard.Server (Server (..))
import Hoard.Types.HoardState (HoardState)


main :: IO ()
main =
    runEff
        . runConcurrent
        . runTimeout
        . runChan
        . runConcNewScope
        . loadOptions
        . runConfigPath
        . loadEnv
        . runConfigReader
        . runHandlesReader
        . Monitoring.runConfig
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
            runSystem
                [ component @Core
                , component @Server
                , component @PeerSharing
                , component @ChainSync
                , component @BlockFetch
                , component @OrphanDetection
                , component @Monitoring
                , component @PeerManager
                ]

            Conc.awaitAll
