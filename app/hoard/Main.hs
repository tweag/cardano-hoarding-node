module Main (main) where

import Data.Default (def)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (evalState)
import Effectful.Temporary (runTemporary)
import Effectful.Timeout (runTimeout)
import Prelude hiding (evalState)

import Hoard.BlockFetch.Events
    ( BlockBatchCompleted
    , BlockFetchFailed
    , BlockFetchRequest
    , BlockFetchStarted
    , BlockReceived
    )
import Hoard.ChainSync.Events (ChainSyncIntersectionFound, ChainSyncStarted, HeaderReceived (..), RollBackward, RollForward)
import Hoard.Component (runSystem)
import Hoard.Control.Exception (runErrorThrowing)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)
import Hoard.Effects.BlockRepo (runBlockRepo)
import Hoard.Effects.Chan (runChan)
import Hoard.Effects.Clock (runClock)
import Hoard.Effects.Conc (runConc)
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
import Hoard.Effects.Verifier (runByronConfig, runShelleyConfig, runVerifier)
import Hoard.Effects.Quota (runQuota)
import Hoard.Effects.WithSocket (withNodeSockets)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered)
import Hoard.KeepAlive.NodeToNode (KeepAlivePing)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed)
import Hoard.Monitoring (Poll)
import Hoard.Network.Events (ProtocolError)
import Hoard.PeerManager (CullRequested, PeerDisconnected, PeerRequested)
import Hoard.PeerManager.Peers (Peers)
import Hoard.PeerSharing.Events (PeerSharingFailed, PeerSharingStarted, PeersReceived)
import Hoard.Types.HoardState (HoardState)

import Hoard.BlockFetch qualified as BlockFetch
import Hoard.ChainSync qualified as ChainSync
import Hoard.Core qualified as Core
import Hoard.Effects.Conc qualified as Conc
import Hoard.Monitoring qualified as Monitoring
import Hoard.OrphanDetection qualified as OrphanDetection
import Hoard.PeerManager qualified as PeerManager
import Hoard.PeerSharing qualified as PeerSharing
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
        . runConfigReader
        . runHandlesReader
        . Monitoring.runConfig
        . runLog
        . runClock
        . runFileSystem
        . runTemporary
        . runByronConfig
        . runShelleyConfig
        . runVerifier
        . runMetrics
        . withNodeSockets
        . runErrorThrowing
        . evalState @HoardState def
        . evalState @Peers def
        . runTracingFromConfig
        . runQuota @(ID Peer, Int64)
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
                [ Core.component
                , Server.component
                , PeerSharing.component
                , ChainSync.component
                , BlockFetch.component
                , OrphanDetection.component
                , Monitoring.component
                , PeerManager.component
                ]
            Conc.awaitAll
