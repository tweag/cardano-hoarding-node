module Hoard.PeerManager (run) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime, diffUTCTime)
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Exception (ExitCase (..), generalBracket)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, gets, modify)
import Prelude hiding (Reader, State, asks, get, gets, modify, state)

import Hoard.Collector (collectFromPeer)
import Hoard.Control.Exception (isGracefulShutdown, withExceptionLogging)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Metrics (Metrics, histogramObserve)
import Hoard.Effects.Metrics.Definitions (metricPeerManagerCullBatches, metricPeerManagerReplenishedCollector)
import Hoard.Effects.NodeToNode (ConnectToError (..), NodeToNode)
import Hoard.Effects.PeerRepo (PeerRepo, updatePeerFailure, upsertPeers)
import Hoard.Effects.PeerRepo qualified as PeerRepo
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Publishing qualified as Sub
import Hoard.KeepAlive.NodeToNode (KeepAlivePing (..))
import Hoard.PeerManager.Config (Config (..))
import Hoard.PeerManager.Peers (Connection (..), ConnectionState (..), Peers (..), mkConnection, signalTermination)
import Hoard.PeerSharing.Events (PeersReceived (..))
import Hoard.Triggers (every)


-- * Main


run
    :: ( BlockRepo :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , Metrics :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , Reader Config :> es
       , State Peers :> es
       , Sub :> es
       )
    => Eff es ()
run = Log.withNamespace "PeerManager" do
    runListeners
    runTriggers


runTriggers
    :: ( Conc :> es
       , Concurrent :> es
       , Pub :> es
       , Reader Config :> es
       , State Peers :> es
       )
    => Eff es ()
runTriggers = do
    triggerCull
    triggerReplenish


runListeners
    :: ( BlockRepo :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , Metrics :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , Reader Config :> es
       , State Peers :> es
       , Sub :> es
       )
    => Eff es ()
runListeners = do
    Conc.fork_ $ Sub.listen updatePeerConnectionState
    Conc.fork_ $ Sub.listen persistReceivedPeers
    Conc.fork_ $ Sub.listen cullOldCollectors
    Conc.fork_ $ Sub.listen noteDisconnectedPeer
    Conc.fork_ $ Sub.listen replenishCollectors


-- * Triggers


triggerCull :: (Concurrent :> es, Conc :> es, Pub :> es) => Eff es ()
triggerCull = every 10 do
    publish CullRequested


triggerReplenish
    :: ( Conc :> es
       , Concurrent :> es
       , Pub :> es
       , Reader Config :> es
       , State Peers :> es
       )
    => Eff es ()
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
    modify $
        Peers
            . Map.adjust (\c -> c {state = Connected}) event.peer.id
            . (.peers)


-- | Processes PeersReceived events and upserts the peer information into
-- the database.
persistReceivedPeers :: (Log :> es, PeerRepo :> es) => PeersReceived -> Eff es ()
persistReceivedPeers event = Log.withNamespace "PeerPersister" do
    Log.info $ "📡 Received " <> show (length event.peerAddresses) <> " peer addresses from remote peer:"
    forM_ event.peerAddresses $ \addr ->
        Log.debug $ "   - " <> show addr.host <> ":" <> show addr.port
    void $ upsertPeers event.peerAddresses event.peer.address event.timestamp


cullOldCollectors
    :: ( Clock :> es
       , Concurrent :> es
       , Log :> es
       , Metrics :> es
       , Reader Config :> es
       , State Peers :> es
       )
    => CullRequested
    -> Eff es ()
cullOldCollectors CullRequested = do
    now <- Clock.currentTime
    maxLifetime <- asks $ (.maxCollectorLifetimeSeconds)
    let isOldConnection = (> maxLifetime) . diffUTCTime now . (.connectedAt)

    oldConnections <- gets $ filter (isOldConnection) . toList . (.peers)

    let numOld = length oldConnections
    if numOld <= 0
        then Log.debug "No old collectors to cull"
        else do
            histogramObserve metricPeerManagerCullBatches $ fromIntegral numOld
            Log.debug $ "Culling " <> show (length oldConnections) <> " old collectors"
    for_ oldConnections signalTermination


noteDisconnectedPeer :: (PeerRepo :> es, Pub :> es) => PeerDisconnected -> Eff es ()
noteDisconnectedPeer event = do
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
       , Pub :> es
       , Reader Config :> es
       , State Peers :> es
       , Sub :> es
       )
    => PeerRequested
    -> Eff es ()
replenishCollectors (PeerRequested n) = do
    Log.debug "Replenishing collectors"
    cooldown <- asks (.peerFailureCooldownSeconds)
    connectedIds <- gets $ Map.keysSet . (.peers)
    potentialPeers <- PeerRepo.getEligiblePeers cooldown connectedIds n
    histogramObserve metricPeerManagerReplenishedCollector $ fromIntegral $ Set.size potentialPeers
    for_ potentialPeers \p -> do
        Log.debug $ "Starting collector for peer " <> show (p.address)
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
       , Log :> es
       , NodeToNode :> es
       , PeerRepo :> es
       , Pub :> es
       , State Peers :> es
       , Sub :> es
       )
    => Peer
    -> Eff es ()
bracketCollector peer = do
    _ <-
        Conc.forkTry @SomeException
            . Log.withNamespace "Collector"
            . withExceptionLogging ("Collector " <> show peer.address)
            $ generalBracket setup cleanup (collectFromPeer peer)
    pure ()
  where
    setup = do
        Log.debug $ "Adding peer to connectedPeers: " <> show peer.address
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
                Log.err $ "Collector exited with exception: " <> show e
            ExitCaseAbort -> do
                timestamp <- Clock.currentTime
                updatePeerFailure peer timestamp
                Log.err $ "Collector exited due to an unknown abort"
            ExitCaseSuccess (Just (ConnectToError errorMessage)) -> do
                timestamp <- Clock.currentTime
                updatePeerFailure peer timestamp
                Log.err $ "Collector exited due to a caught exception: " <> errorMessage
            ExitCaseSuccess Nothing ->
                Log.debug $ "Collector terminated while operating normally: " <> show peer.address

        signalTermination conn
        Log.debug $ "Removing peer from peers state: " <> show peer.address
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
