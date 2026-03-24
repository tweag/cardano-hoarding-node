module Main (main) where

import Data.Default (def)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.Reader.Static (ask)
import Effectful.State.Static.Shared (evalState)
import Effectful.Temporary (runTemporary)
import Effectful.Timeout (runTimeout)

import Atelier.Component (runSystem)
import Atelier.Effects.Chan (runChan)
import Atelier.Effects.Clock (runClock)
import Atelier.Effects.Conc.Traced (runConcByConfig)
import Atelier.Effects.Delay (runDelay)
import Atelier.Effects.Log (runLog)
import Atelier.Effects.Monitoring.Metrics (runMetrics)
import Atelier.Effects.Monitoring.Tracing (TracingConfig, runTracingFromConfig)
import Atelier.Effects.Publishing (runPubSub)
import Atelier.Effects.Tally (runTally)
import Atelier.Effects.UUID (runGenUUID)
import Hoard.Control.Exception (runErrorThrowing)
import Hoard.Effects.BlockRepo (runBlockRepo)
import Hoard.Effects.ChainDB (runChainDB)
import Hoard.Effects.ConfigPath (runConfig, runConfigRoot)
import Hoard.Effects.DB (runDB)
import Hoard.Effects.Environment (loadEnv)
import Hoard.Effects.HeaderRepo (runHeaderRepo)
import Hoard.Effects.HoardStateRepo (runHoardStateRepo)
import Hoard.Effects.NodeToClient (runNodeToClient)
import Hoard.Effects.NodeToNode (runNodeToNode)
import Hoard.Effects.Options (loadOptions)
import Hoard.Effects.PeerNoteRepo (runPeerNoteRepo)
import Hoard.Effects.PeerRepo (runPeerRepo)
import Hoard.Effects.Verifier (runByronConfig, runShelleyConfig, runVerifier)
import Hoard.PeerManager.Peers (Peers)
import Hoard.Types.HoardState (HoardState)

import Atelier.Effects.Cache.Config qualified as Cache
import Atelier.Effects.Conc qualified as Conc
import Atelier.Effects.Log qualified as Log
import Hoard.CardanoNode.Config qualified as CardanoNode
import Hoard.ChainDB qualified as ChainDB
import Hoard.ChainDB.Events qualified as ChainDB
import Hoard.Core qualified as Core
import Hoard.Effects.ChainDB qualified as ChainDB
import Hoard.Effects.NodeToNode.Config qualified as NodeToNode
import Hoard.Effects.WithSocket qualified as WithSocket
import Hoard.Events.BlockFetch qualified as BlockFetch
import Hoard.Events.ChainSync qualified as ChainSync
import Hoard.Events.ImmutableTipRefreshTriggered qualified as NodeToClient
import Hoard.Events.KeepAlive qualified as KeepAlive
import Hoard.Events.PeerSharing qualified as PeerSharing
import Hoard.Eviction qualified as Eviction
import Hoard.ImmutableTip qualified as ImmutableTip
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
        . runDelay
        . runTimeout
        . runChan
        . loadOptions
        . runConfigRoot
        . loadEnv
        . runConfig @"server" @Server.Config
        . runConfig @"monitoring" @Monitoring.Config
        . runConfig @"cardano_protocols" @NodeToNode.ProtocolsConfig
        . runConfig @"node_to_node" @NodeToNode.NodeToNodeConfig
        . runConfig @"orphan_detection" @OrphanDetection.Config
        . runConfig @"eviction" @Eviction.Config
        . runConfig @"cardano_node_integration" @CardanoNode.Config
        . runConfig @"cardano_node_integration" @ImmutableTip.Config
        . runConfig @"node_sockets" @WithSocket.NodeSocketsConfig
        . runConfig @"chain_db" @ChainDB.Config
        . runConfig @"peer_manager" @PeerManager.Config
        . runConfig @"quota" @Cache.Config
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
        . Sentry.runDuplicateBlocksReader
        . runConcByConfig
        . runTally @Persistence.PeerSlotKey
        . runTally @Sentry.DuplicateBlocksKey
        . runPubSub @BlockFetch.BatchCompleted
        . runPubSub @BlockFetch.BatchFailed
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
        . runPubSub @NodeToClient.ImmutableTipRefreshTriggered
        . runPubSub @ImmutableTip.Refreshed
        . runPubSub @PeerManager.CollectorRequested
        . runPubSub @PeerManager.CullRequested
        . runPubSub @PeerManager.PeerDisconnected
        . runPubSub @PeerManager.PeerRequested
        . runPubSub @PeerSharing.PeersReceived
        . runPubSub @ChainDB.ChainExtended
        . runPubSub @ChainDB.BlockSealed
        . runPubSub @ChainDB.BlockRejected
        . runPubSub @ChainDB.BlockRolledBack
        . runPubSub @Sentry.AdversarialBehavior
        . runPubSub @Sentry.ReceivedBlockOutsideRequestedRange
        . runPubSub @Sentry.ReceivedMismatchingBlock
        . runGenUUID
        . runChainDB
        . runNodeToClient
        . runNodeToNode
        . runDB
        . runHeaderRepo
        . runPeerRepo
        . runBlockRepo
        . runHoardStateRepo
        . runPeerNoteRepo
        $ do
            chainDBCfg <- ask @ChainDB.Config
            runSystem
                $ [ Core.component
                  , ImmutableTip.component
                  , Sentry.component
                  , Server.component
                  , Persistence.component
                  , OrphanDetection.component
                  , Eviction.component
                  , Monitoring.component
                  , PeerManager.component
                  ]
                    <> [ChainDB.component | chainDBCfg.enabled]
            Conc.awaitAll
