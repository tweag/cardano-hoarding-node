module Hoard.PeerManager
    ( component
    , Config (..)
    , PeerRequested (..)
    , CullRequested (..)
    , PeerDisconnected (..)
    ) where

import Data.Time (UTCTime, diffUTCTime)
import Effectful (IOE)
import Effectful.Concurrent (Concurrent)
import Effectful.Exception (ExitCase (..), generalBracket)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, gets, modify)

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Hoard.Bootstrap (bootstrapPeers)
import Hoard.Collector (collectFromPeer)
import Hoard.Component (Component (..), defaultComponent)
import Hoard.Control.Exception (isGracefulShutdown, withExceptionLogging)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Metrics (Metrics, histogramObserve)
import Hoard.Effects.Monitoring.Metrics.Definitions (metricPeerManagerCullBatches, metricPeerManagerReplenishedCollector)
import Hoard.Effects.Monitoring.Tracing (SpanStatus (..), Tracing, addAttribute, setStatus, withSpan)
import Hoard.Effects.NodeToNode (ConnectToError (..), NodeToNode)
import Hoard.Effects.PeerRepo (PeerRepo, updatePeerFailure)
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Verifier (Verifier)
import Hoard.PeerManager.Config (Config (..))
import Hoard.PeerManager.Peers (Connection (..), ConnectionState (..), Peers (..), mkConnection, signalTermination)
import Hoard.Sentry (AdversarialBehavior (..))
import Hoard.Triggers (every)
import Hoard.Types.Environment (PeerSnapshotFile)

import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.PeerRepo qualified as PeerRepo
import Hoard.Effects.Publishing qualified as Sub
import Hoard.Events.BlockFetch qualified as BlockFetch
import Hoard.Events.ChainSync qualified as ChainSync
import Hoard.Events.KeepAlive qualified as KeepAlive
import Hoard.Sentry qualified as Sentry


-- * Component


component
    :: ( BlockRepo :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Log :> es
       , Metrics :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub BlockFetch.Request :> es
       , Pub CullRequested :> es
       , Pub PeerDisconnected :> es
       , Pub PeerRequested :> es
       , Reader Config :> es
       , Reader PeerSnapshotFile :> es
       , State Peers :> es
       , Sub AdversarialBehavior :> es
       , Sub ChainSync.HeaderReceived :> es
       , Sub CullRequested :> es
       , Sub KeepAlive.Ping :> es
       , Sub PeerDisconnected :> es
       , Sub PeerRequested :> es
       , Tracing :> es
       , Verifier :> es
       )
    => Component es
component =
    defaultComponent
        { name = "PeerManager"
        , setup =
            void bootstrapPeers
        , listeners =
            pure
                [ Sub.listen updatePeerConnectionState
                , Sub.listen cullOldCollectors
                , Sub.listen noteDisconnectedPeer
                , Sub.listen replenishCollectors
                , Sub.listen cullAdversarialPeers
                ]
        , triggers =
            pure
                [ triggerCull
                , triggerReplenish
                ]
        }


-- * Triggers


triggerCull :: (Concurrent :> es, Pub CullRequested :> es) => Eff es Void
triggerCull = every 10 do
    publish CullRequested


triggerReplenish
    :: ( Concurrent :> es
       , Pub PeerRequested :> es
       , Reader Config :> es
       , State Peers :> es
       )
    => Eff es Void
triggerReplenish = do
    replenish
    every 20 replenish
  where
    replenish = do
        limit <- asks $ fromIntegral . (.maxConcurrentCollectors)
        peers <- gets $ Map.size . (.peers)
        let missingPeers = limit - peers
        when (missingPeers > 0) do
            publish $ PeerRequested $ fromIntegral $ missingPeers


-- * Listeners


updatePeerConnectionState :: (State Peers :> es) => KeepAlive.Ping -> Eff es ()
updatePeerConnectionState event =
    modify
        $ Peers
            . Map.adjust (\c -> c {state = Connected}) event.peer.id
            . (.peers)


cullOldCollectors
    :: ( Clock :> es
       , Concurrent :> es
       , Metrics :> es
       , Reader Config :> es
       , State Peers :> es
       , Tracing :> es
       )
    => CullRequested
    -> Eff es ()
cullOldCollectors CullRequested = withSpan "peer_manager.cull_old_collectors" do
    now <- Clock.currentTime
    maxLifetime <- asks $ (.maxCollectorLifetimeSeconds)
    let isOldConnection = (> maxLifetime) . diffUTCTime now . (.connectedAt)

    oldConnections <- gets $ filter (isOldConnection) . toList . (.peers)

    let numOld = length oldConnections
    addAttribute "collectors.old.count" numOld
    when (numOld > 0)
        $ histogramObserve metricPeerManagerCullBatches
        $ fromIntegral numOld
    withSpan "terminate_connections"
        $ for_ oldConnections signalTermination


cullAdversarialPeers
    :: ( Concurrent :> es
       , State Peers :> es
       , Tracing :> es
       )
    => AdversarialBehavior -> Eff es ()
cullAdversarialPeers event = do
    when (event.severity == Sentry.Critical) $ withSpan "peer_manager.cull_adversarial_peers" do
        conn <- gets $ fmap snd . find ((event.peer.id ==) . fst) . Map.toList . (.peers)
        _ <- traverse signalTermination conn
        pure ()


noteDisconnectedPeer :: (PeerRepo :> es, Pub PeerRequested :> es, Tracing :> es) => PeerDisconnected -> Eff es ()
noteDisconnectedPeer event = withSpan "peer_manager.note_disconnected_peer" do
    PeerRepo.updateLastConnected event.peerId event.timestamp
    publish $ PeerRequested 1


replenishCollectors
    :: ( BlockRepo :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , Metrics :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub BlockFetch.Request :> es
       , Pub PeerDisconnected :> es
       , Reader Config :> es
       , State Peers :> es
       , Sub ChainSync.HeaderReceived :> es
       , Tracing :> es
       , Verifier :> es
       )
    => PeerRequested
    -> Eff es ()
replenishCollectors (PeerRequested n) = withSpan "peer_manager.replenish_collectors" do
    addAttribute "collectors.requested.count" $ fromIntegral @_ @Int n
    cooldown <- asks (.peerFailureCooldownSeconds)
    connectedIds <- gets $ Map.keysSet . (.peers)
    potentialPeers <- PeerRepo.getEligiblePeers cooldown connectedIds n
    let actualCount = Set.size potentialPeers
    addAttribute "peers.eligible.count" actualCount
    histogramObserve metricPeerManagerReplenishedCollector $ fromIntegral actualCount
    withSpan "start_collectors"
        $ for_ potentialPeers bracketCollector


-- * Collector utilities


-- | Fork a collector with exception handling and state management.
--
-- This wraps collectFromPeer with:
-- - Proper exception handling (forkTry + withExceptionLogging)
-- - Connected peers state management (adds peer on start, removes on termination)
-- - Failure time tracking (updates peer's lastFailureTime on exception)
--
-- The peer is automatically removed from the `peers` state when the collector
-- terminates, whether due to an exception or normal completion.
bracketCollector
    :: ( BlockRepo :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub BlockFetch.Request :> es
       , Pub PeerDisconnected :> es
       , State Peers :> es
       , Sub ChainSync.HeaderReceived :> es
       , Tracing :> es
       , Verifier :> es
       )
    => Peer
    -> Eff es ()
bracketCollector peer = withSpan "peer_manager.bracket_collector" do
    addAttribute "peer.address" peer.address
    _ <-
        Conc.forkTry @SomeException
            . withExceptionLogging ("Collector " <> show peer.address)
            $ generalBracket createConn cleanup (collectFromPeer peer)
    pure ()
  where
    createConn = withSpan "peer_manager.create_connection" do
        conn <- mkConnection
        modify $ coerce $ Map.insert peer.id conn
        pure conn

    cleanup conn exitCase = withSpan "peer_manager.connection_terminated" do
        case exitCase of
            -- Only update failure time for real errors, not clean shutdowns
            ExitCaseException e -> do
                unless (isGracefulShutdown e) do
                    timestamp <- Clock.currentTime
                    updatePeerFailure peer timestamp
                setStatus $ Error $ "Collector exception: " <> show e
            ExitCaseAbort -> do
                timestamp <- Clock.currentTime
                updatePeerFailure peer timestamp
                setStatus $ Error "Collector aborted"
            ExitCaseSuccess (Just (ConnectToError errorMessage)) -> do
                timestamp <- Clock.currentTime
                updatePeerFailure peer timestamp
                setStatus $ Error $ "Collector error: " <> errorMessage
            ExitCaseSuccess Nothing ->
                setStatus Ok

        signalTermination conn
        modify $ Peers . Map.delete peer.id . (.peers)
        timestamp <- Clock.currentTime
        publish
            PeerDisconnected
                { peerId = peer.id
                , timestamp
                }


-- * Events


data PeerRequested = PeerRequested Word
    deriving (Show, Typeable)


data CullRequested = CullRequested
    deriving (Show, Typeable)


data PeerDisconnected = PeerDisconnected
    { peerId :: (ID Peer)
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)
