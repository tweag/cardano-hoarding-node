module Main (main) where

import Data.Default (def)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (evalState)
import Effectful.Temporary (runTemporary)
import Effectful.Timeout (runTimeout)
import Prelude hiding (evalState)

import Hoard.Component (runSystem)
import Hoard.Control.Exception (runErrorThrowing)
import Hoard.Effects.BlockRepo (runBlockRepo)
import Hoard.Effects.Chan (runChan)
import Hoard.Effects.Clock (runClock)
import Hoard.Effects.Conc (runConc)
import Hoard.Effects.ConfigPath (runConfig, runConfigPath)
import Hoard.Effects.DBRead (runDBRead)
import Hoard.Effects.DBWrite (runDBWrite)
import Hoard.Effects.Environment (loadEnv)
import Hoard.Effects.HeaderRepo (runHeaderRepo)
import Hoard.Effects.HoardStateRepo (runHoardStateRepo)
import Hoard.Effects.Log (runLog)
import Hoard.Effects.Monitoring.Metrics (runMetrics)
import Hoard.Effects.Monitoring.Tracing (TracingConfig, runTracingFromConfig)
import Hoard.Effects.NodeToClient (runNodeToClient)
import Hoard.Effects.NodeToNode (runNodeToNode)
import Hoard.Effects.Options (loadOptions)
import Hoard.Effects.PeerNoteRepo (runPeerNoteRepo)
import Hoard.Effects.PeerRepo (runPeerRepo)
import Hoard.Effects.Publishing (runPubSub)
import Hoard.Effects.Quota (runQuota)
import Hoard.Effects.UUID (runGenUUID)
import Hoard.Effects.Verifier (runByronConfig, runShelleyConfig, runVerifier)
import Hoard.Events.ChainSync (ChainSyncIntersectionFound, ChainSyncStarted, HeaderReceived (..), RollBackward, RollForward)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered)
import Hoard.Events.KeepAlive (KeepAlivePing)
import Hoard.Events.Network (ProtocolError)
import Hoard.Events.PeerSharing (PeerSharingFailed, PeerSharingStarted, PeersReceived)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed)
import Hoard.Monitoring (Poll)
import Hoard.PeerManager (CullRequested, PeerDisconnected, PeerRequested)
import Hoard.PeerManager.Peers (Peers)
import Hoard.Persistence (PeerSlotKey)
import Hoard.Types.HoardState (HoardState)

import Hoard.BlockEviction qualified as BlockEviction
import Hoard.CardanoNode.Config qualified as CardanoNode
import Hoard.Core qualified as Core
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode.Config qualified as NodeToNode
import Hoard.Effects.Quota.Config qualified as Quota
import Hoard.Effects.WithSocket qualified as WithSocket
import Hoard.Events.BlockFetch qualified as BlockFetch
import Hoard.Monitoring qualified as Monitoring
import Hoard.OrphanDetection qualified as OrphanDetection
import Hoard.PeerManager qualified as PeerManager
import Hoard.Persistence qualified as Persistence
import Hoard.Sentry qualified as Sentry
import Hoard.Server qualified as Server


main :: IO ()
main =
    runEff
        . runConcurrent
        . runTimeout
        . runChan
        . runConc
        . loadOptions
        . runConfigPath
        . loadEnv
        . runConfig @"server" @Server.Config
        . runConfig @"monitoring" @Monitoring.Config
        . runConfig @"cardano_protocols" @NodeToNode.ProtocolsConfig
        . runConfig @"node_to_node" @NodeToNode.NodeToNodeConfig
        . runConfig @"block_eviction" @BlockEviction.Config
        . runConfig @"cardano_node_integration" @CardanoNode.Config
        . runConfig @"node_sockets" @WithSocket.NodeSocketsConfig
        . runConfig @"peer_manager" @PeerManager.Config
        . runConfig @"quota" @Quota.Config
        . runConfig @"setup" @Core.SetupConfig
        . runConfig @"logging" @Log.Config
        . runConfig @"tracing" @TracingConfig
        . runConfig @"sentry" @Sentry.Config
        . runLog
        . runClock
        . runFileSystem
        . runTemporary
        . runByronConfig
        . runShelleyConfig
        . runVerifier
        . runMetrics
        . WithSocket.withNodeSockets
        . runErrorThrowing
        . evalState @HoardState def
        . evalState @Peers def
        . Sentry.runDuplicateBlocksState
        . runTracingFromConfig
        . runQuota @PeerSlotKey 1
        . runPubSub @ChainSyncStarted
        . runPubSub @ChainSyncIntersectionFound
        . runPubSub @RollBackward
        . runPubSub @RollForward
        . runPubSub @HeaderReceived
        . runPubSub @BlockFetch.RequestStarted
        . runPubSub @BlockFetch.Request
        . runPubSub @BlockFetch.BlockReceived
        . runPubSub @BlockFetch.BatchCompleted
        . runPubSub @BlockFetch.RequestFailed
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
        . runPubSub @Sentry.AdversarialBehavior
        . runGenUUID
        . runNodeToClient
        . runNodeToNode
        . runDBRead
        . runDBWrite
        . runHeaderRepo
        . runPeerRepo
        . runBlockRepo
        . runHoardStateRepo
        . runPeerNoteRepo
        $ do
            runSystem
                [ Core.component
                , Sentry.component
                , Server.component
                , Persistence.component
                , OrphanDetection.component
                , BlockEviction.component
                , Monitoring.component
                , PeerManager.component
                ]
            Conc.awaitAll
