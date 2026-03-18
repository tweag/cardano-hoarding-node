module Hoard.PeerManager
    ( component
    , Config (..)
    , CollectorRequested (..)
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

import Atelier.Component (Component (..), defaultComponent)
import Atelier.Effects.Clock (Clock)
import Atelier.Effects.Conc (Conc)
import Atelier.Effects.Log (Log)
import Atelier.Effects.Monitoring.Metrics (Metrics, histogramObserve)
import Atelier.Effects.Monitoring.Tracing (SpanStatus (..), Tracing, addAttribute, setStatus, withSpan)
import Atelier.Effects.Publishing (Pub, Sub, publish)
import Hoard.Bootstrap (bootstrapPeers, bootstrapPinnedPeers)
import Hoard.Collector (collectFromPeer)
import Hoard.Control.Exception (isGracefulShutdown, withExceptionLogging)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Monitoring.Metrics.Definitions (metricPeerConnectionEstablishment, metricPeerManagerCullBatches, metricPeerManagerReplenishedCollector)
import Hoard.Effects.NodeToNode (ConnectToError (..), NodeToNode)
import Hoard.Effects.PeerRepo (PeerRepo, updatePeerFailure)
import Hoard.Effects.Verifier (Verifier)
import Hoard.PeerManager.Config (AutomaticConfig (..), Config (..), PeerMode (..))
import Hoard.PeerManager.Peers (Connection (..), ConnectionState (..), Peers (..), mkConnection, signalTermination)
import Hoard.Sentry (AdversarialBehavior (..))
import Hoard.Triggers (every)
import Hoard.Types.Environment (PeerSnapshotFile)

import Atelier.Effects.Clock qualified as Clock
import Atelier.Effects.Conc qualified as Conc
import Atelier.Effects.Log qualified as Log
import Atelier.Effects.Publishing qualified as Sub
import Hoard.Effects.PeerRepo qualified as PeerRepo
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
       , Pub CollectorRequested :> es
       , Pub CullRequested :> es
       , Pub PeerDisconnected :> es
       , Pub PeerRequested :> es
       , Reader Config :> es
       , Reader PeerSnapshotFile :> es
       , State Peers :> es
       , Sub AdversarialBehavior :> es
       , Sub ChainSync.HeaderReceived :> es
       , Sub CollectorRequested :> es
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
        , setup = do
            mode <- asks @Config (.peerMode)
            case mode of
                Automatic AutomaticConfig {bootstrapPins} -> do
                    knownPeersExist <- PeerRepo.hasPeers
                    if knownPeersExist then
                        Log.debug "Known peers found in database, skipping bootstrap"
                    else do
                        Log.debug "No known peers found, bootstrapping from peer snapshot"
                        bootstrappedPeers <- bootstrapPeers
                        Log.debug $ "Bootstrapped " <> show (Set.size bootstrappedPeers) <> " peers from peer snapshot"
                    when bootstrapPins do
                        pinnedPeers <- PeerRepo.getPinnedPeers
                        when (null pinnedPeers) do
                            Log.debug "No pinned peers found, bootstrapping pinned peers from peer snapshot"
                            bootstrappedPinnedPeers <- bootstrapPinnedPeers
                            Log.debug $ "Bootstrapped " <> show (length bootstrappedPinnedPeers) <> " pinned peers from peer snapshot"
                Manual -> do
                    pinnedPeers <- PeerRepo.getPinnedPeers
                    if not (null pinnedPeers) then
                        Log.debug "Pinned peers found, skipping bootstrap"
                    else do
                        Log.debug "No pinned peers found, bootstrapping from peer snapshot"
                        bootstrappedPeers <- bootstrapPinnedPeers
                        Log.debug $ "Bootstrapped " <> show (length bootstrappedPeers) <> " pinned peers from peer snapshot"
        , listeners =
            pure
                [ Sub.listen updatePeerConnectionState
                , Sub.listen_ cullOldCollectors
                , Sub.listen noteDisconnectedPeer
                , Sub.listen_ replenishCollectors
                , Sub.listen_ startCollector
                , Sub.listen_ cullAdversarialPeers
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
    interval <- asks (.replenishIntervalSeconds)
    every interval replenish
  where
    replenish = do
        limit <- asks $ fromIntegral . (.maxConcurrentCollectors)
        peers <- gets $ Map.size . (.peers)
        let missingPeers = limit - peers
        when (missingPeers > 0) do
            publish $ PeerRequested $ fromIntegral $ missingPeers


-- * Listeners


updatePeerConnectionState :: (Metrics :> es, State Peers :> es) => UTCTime -> KeepAlive.Ping -> Eff es ()
updatePeerConnectionState timestamp event = do
    conn <- gets $ Map.lookup event.peer.id . (.peers)
    forM_ conn \c ->
        when (c.state == Connecting)
            $ histogramObserve metricPeerConnectionEstablishment
            $ realToFrac
            $ diffUTCTime timestamp c.connectedAt
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


noteDisconnectedPeer :: (PeerRepo :> es, Tracing :> es) => UTCTime -> PeerDisconnected -> Eff es ()
noteDisconnectedPeer timestamp event = withSpan "peer_manager.note_disconnected_peer" do
    PeerRepo.updateLastConnected event.peerId timestamp


replenishCollectors
    :: ( Metrics :> es
       , PeerRepo :> es
       , Pub CollectorRequested :> es
       , Reader Config :> es
       , State Peers :> es
       , Tracing :> es
       )
    => PeerRequested
    -> Eff es ()
replenishCollectors (PeerRequested n) = withSpan "peer_manager.replenish_collectors" do
    addAttribute "collectors.requested.count" $ fromIntegral @_ @Int n
    cooldown <- asks (.peerFailureCooldownSeconds)
    mode <- asks @Config (.peerMode)
    connectedIds <- gets $ Map.keysSet . (.peers)
    -- Always try pinned peers first
    pinnedPeers <- PeerRepo.getEligiblePinnedPeers cooldown connectedIds n
    let pinnedCount = fromIntegral (Set.size pinnedPeers)
    -- In Automatic mode, fill any remaining slots with general eligible peers
    additionalPeers <- case mode of
        Automatic _ | pinnedCount < n -> do
            let alreadyHandled = connectedIds `Set.union` Set.map (.id) pinnedPeers
            PeerRepo.getEligiblePeers cooldown alreadyHandled (n - pinnedCount)
        _ -> pure mempty
    let potentialPeers = pinnedPeers `Set.union` additionalPeers
    let actualCount = Set.size potentialPeers
    addAttribute "peers.eligible.count" actualCount
    histogramObserve metricPeerManagerReplenishedCollector $ fromIntegral actualCount
    for_ potentialPeers \peer -> publish CollectorRequested {peer}


startCollector
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
    => CollectorRequested
    -> Eff es ()
startCollector CollectorRequested {peer} = bracketCollector peer


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
        publish
            PeerDisconnected
                { peerId = peer.id
                }


-- * Events


data CollectorRequested = CollectorRequested
    { peer :: Peer
    }


data PeerRequested = PeerRequested Word


data CullRequested = CullRequested
    deriving stock (Show)


data PeerDisconnected = PeerDisconnected
    { peerId :: (ID Peer)
    }
