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
import Hoard.PeerManager.Peers (Peers)
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
import Hoard.Events.ChainSync qualified as ChainSync
import Hoard.Events.ImmutableTipRefreshTriggered qualified as NodeToClient
import Hoard.Events.KeepAlive qualified as KeepAlive
import Hoard.Events.Network qualified as Network
import Hoard.Events.PeerSharing qualified as PeerSharing
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener qualified as NodeToClient
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
        . runTracingFromConfig
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
        . runConc
        . runQuota @Persistence.PeerSlotKey 1
        . runPubSub @BlockFetch.BatchCompleted
        . runPubSub @BlockFetch.BlockReceived
        . runPubSub @BlockFetch.Request
        . runPubSub @BlockFetch.RequestFailed
        . runPubSub @BlockFetch.RequestStarted
        . runPubSub @ChainSync.HeaderReceived
        . runPubSub @ChainSync.IntersectionFound
        . runPubSub @ChainSync.RollBackward
        . runPubSub @ChainSync.RollForward
        . runPubSub @KeepAlive.Ping
        . runPubSub @Monitoring.Poll
        . runPubSub @Network.ProtocolError
        . runPubSub @NodeToClient.ImmutableTipRefreshTriggered
        . runPubSub @NodeToClient.ImmutableTipRefreshed
        . runPubSub @PeerManager.CullRequested
        . runPubSub @PeerManager.PeerDisconnected
        . runPubSub @PeerManager.PeerRequested
        . runPubSub @PeerSharing.PeersReceived
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
