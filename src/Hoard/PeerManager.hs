module Hoard.PeerManager
    ( component
    , PeerRequested (..)
    , CullRequested (..)
    , PeerDisconnected (..)
    ) where

import Data.Time (UTCTime, diffUTCTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Exception (ExitCase (..), generalBracket)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, gets, modify)
import Prelude hiding (Reader, State, asks, get, gets, modify, state)

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Hoard.BlockFetch.Events (BlockFetchRequest)
import Hoard.Bootstrap (bootstrapPeers)
import Hoard.Collector (collectFromPeer)
import Hoard.Component (Component (..), defaultComponent)
import Hoard.Control.Exception (isGracefulShutdown, withExceptionLogging)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Monitoring.Metrics (Metrics, histogramObserve)
import Hoard.Effects.Monitoring.Metrics.Definitions (metricPeerManagerCullBatches, metricPeerManagerReplenishedCollector)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, addEvent, withSpan)
import Hoard.Effects.NodeToNode (ConnectToError (..), NodeToNode)
import Hoard.Effects.PeerRepo (PeerRepo, updatePeerFailure, upsertPeers)
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Verifier (Verifier)
import Hoard.KeepAlive.NodeToNode (KeepAlivePing (..))
import Hoard.PeerManager.Config (Config (..))
import Hoard.PeerManager.Peers (Connection (..), ConnectionState (..), Peers (..), mkConnection, signalTermination)
import Hoard.PeerSharing.Events (PeersReceived (..))
import Hoard.Triggers (every)

import Hoard.ChainSync.Events qualified as ChainSync
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.PeerRepo qualified as PeerRepo
import Hoard.Effects.Publishing qualified as Sub
import Hoard.Types.Environment qualified as Env


-- * Component


component
    :: ( BlockRepo :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Metrics :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub BlockFetchRequest :> es
       , Pub CullRequested :> es
       , Pub PeerDisconnected :> es
       , Pub PeerRequested :> es
       , Reader Config :> es
       , Reader Env.Config :> es
       , State Peers :> es
       , Sub ChainSync.HeaderReceived :> es
       , Sub CullRequested :> es
       , Sub KeepAlivePing :> es
       , Sub PeerDisconnected :> es
       , Sub PeerRequested :> es
       , Sub PeersReceived :> es
       , Tracing :> es
       , Verifier :> es
       )
    => Component es
component =
    defaultComponent
        { name = "PeerManager"
        , setup = withSpan "peer_manager:setup" $ do
            addEvent "bootstrapping_peers" []
            void bootstrapPeers
            addEvent "peers_bootstrapped" []
        , listeners =
            pure
                [ Sub.listen updatePeerConnectionState
                , Sub.listen persistReceivedPeers
                , Sub.listen cullOldCollectors
                , Sub.listen noteDisconnectedPeer
                , Sub.listen replenishCollectors
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


updatePeerConnectionState :: (State Peers :> es) => KeepAlivePing -> Eff es ()
updatePeerConnectionState event =
    modify
        $ Peers
            . Map.adjust (\c -> c {state = Connected}) event.peer.id
            . (.peers)


-- | Processes PeersReceived events and upserts the peer information into
-- the database.
persistReceivedPeers :: (PeerRepo :> es, Tracing :> es) => PeersReceived -> Eff es ()
persistReceivedPeers event = withSpan "persist_received_peers" do
    addAttribute "peers.count" (show $ length event.peerAddresses)
    addAttribute "source.peer" (show event.peer.address)
    addEvent "persisting_peers" [("count", show $ length event.peerAddresses)]
    forM_ event.peerAddresses $ \addr ->
        addEvent "peer_address" [("host", show addr.host), ("port", show addr.port)]
    void $ upsertPeers event.peerAddresses event.peer.address event.timestamp
    addEvent "peers_persisted" []


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
cullOldCollectors CullRequested = withSpan "cull_old_collectors" do
    now <- Clock.currentTime
    maxLifetime <- asks $ (.maxCollectorLifetimeSeconds)
    let isOldConnection = (> maxLifetime) . diffUTCTime now . (.connectedAt)

    oldConnections <- gets $ filter (isOldConnection) . toList . (.peers)

    let numOld = length oldConnections
    addAttribute "old_collectors.count" (show numOld)
    if numOld <= 0 then
        addEvent "no_collectors_to_cull" []
    else do
        histogramObserve metricPeerManagerCullBatches $ fromIntegral numOld
        addEvent "culling_collectors" [("count", show numOld)]
    for_ oldConnections signalTermination


noteDisconnectedPeer :: (PeerRepo :> es, Pub PeerRequested :> es) => PeerDisconnected -> Eff es ()
noteDisconnectedPeer event = do
    PeerRepo.updateLastConnected event.peerId event.timestamp
    publish $ PeerRequested 1


replenishCollectors
    :: ( BlockRepo :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Metrics :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub BlockFetchRequest :> es
       , Pub PeerDisconnected :> es
       , Reader Config :> es
       , State Peers :> es
       , Sub ChainSync.HeaderReceived :> es
       , Tracing :> es
       , Verifier :> es
       )
    => PeerRequested
    -> Eff es ()
replenishCollectors (PeerRequested n) = withSpan "replenish_collectors" do
    addAttribute "requested.count" (show n)
    addEvent "replenishing" [("requested", show n)]
    cooldown <- asks (.peerFailureCooldownSeconds)
    connectedIds <- gets $ Map.keysSet . (.peers)
    potentialPeers <- PeerRepo.getEligiblePeers cooldown connectedIds n
    let actualCount = Set.size potentialPeers
    addAttribute "eligible.count" (show actualCount)
    histogramObserve metricPeerManagerReplenishedCollector $ fromIntegral actualCount
    for_ potentialPeers \p -> do
        addEvent "starting_collector" [("peer", show p.address)]
        bracketCollector p


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
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub BlockFetchRequest :> es
       , Pub PeerDisconnected :> es
       , State Peers :> es
       , Sub ChainSync.HeaderReceived :> es
       , Tracing :> es
       , Verifier :> es
       )
    => Peer
    -> Eff es ()
bracketCollector peer = do
    _ <-
        Conc.forkTry @SomeException
            . withSpan "collector"
            . withExceptionLogging ("Collector " <> show peer.address)
            $ generalBracket connect cleanup (collectFromPeer peer)
    pure ()
  where
    connect = do
        addEvent "adding_peer" [("peer", show peer.address)]
        conn <- mkConnection
        modify $ coerce $ Map.insert peer.id conn
        pure conn

    cleanup conn exitCase = do
        case exitCase of
            -- Only update failure time for real errors, not clean shutdowns
            ExitCaseException e -> do
                unless (isGracefulShutdown e) do
                    timestamp <- Clock.currentTime
                    updatePeerFailure peer timestamp
                addEvent "collector_exception" [("peer", show peer.address), ("error", show e)]
            ExitCaseAbort -> do
                timestamp <- Clock.currentTime
                updatePeerFailure peer timestamp
                addEvent "collector_aborted" [("peer", show peer.address)]
            ExitCaseSuccess (Just (ConnectToError errorMessage)) -> do
                timestamp <- Clock.currentTime
                updatePeerFailure peer timestamp
                addEvent "collector_error" [("peer", show peer.address), ("error", errorMessage)]
            ExitCaseSuccess Nothing ->
                addEvent "collector_terminated_normally" [("peer", show peer.address)]

        signalTermination conn
        addEvent "removing_peer" [("peer", show peer.address)]
        modify $ Peers . Map.delete peer.id . (.peers)
        timestamp <- Clock.currentTime
        publish
            PeerDisconnected
                { peerId = peer.id
                , timestamp
                }


-- * Events


data PeerRequested = PeerRequested Word


data CullRequested = CullRequested
    deriving (Show, Typeable)


data PeerDisconnected = PeerDisconnected
    { peerId :: (ID Peer)
    , timestamp :: UTCTime
    }
