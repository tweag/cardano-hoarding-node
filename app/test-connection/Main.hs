-- |
-- Module: Main
-- Description: Simple test program to verify peer connection functionality
--
-- This executable connects to a Preview testnet relay and verifies that:
-- 1. The connection is established successfully
-- 2. Version negotiation completes
-- 3. The connection stays alive
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan.Unagi (newChan)
import Data.IP (IP)
import Data.IP qualified as IP
import Data.Time (getCurrentTime)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Network.Socket (PortNumber)
import Network.Socket qualified as Socket
import Ouroboros.Network.IOManager (withIOManager)

import Data.UUID qualified as UUID

import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Conc (Conc, scoped)
import Hoard.Effects.Log (Log, runLog)
import Hoard.Effects.Network (Network, connectToPeer, isConnected, runNetwork)
import Hoard.Effects.Pub (runPub)
import Hoard.Effects.Sub (Sub, listen, runSub)
import Hoard.Network.Events
import Hoard.Types.NodeIP (NodeIP (..))

import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log qualified as Log


main :: IO ()
main = withIOManager $ \ioManager -> do
    putTextLn "=== Cardano Peer Connection Test ==="
    putTextLn ""
    putTextLn "This program will:"
    putTextLn "  1. Connect to a Preview testnet relay"
    putTextLn "  2. Verify handshake completion"
    putTextLn "  3. Request peer addresses via PeerSharing protocol"
    putTextLn "  4. Receive headers via ChainSync protocol"
    putTextLn "  5. Keep the connection alive for 60 seconds"
    putTextLn ""

    -- Create event channel
    (inChan, _outChan) <- newChan

    -- Run the test
    result <- runEff
        . runLog
        . runConcurrent
        . scoped
        $ \scope -> do
            Conc.runConcWithKi scope
                . runErrorNoCallStack @Text
                . runSub inChan
                . runPub inChan
                . runNetwork ioManager inChan "config/preview/config.json"
                $ testConnection

    case result of
        Left err -> do
            putTextLn $ "\n❌ Test failed with error: " <> err
        Right () -> do
            putTextLn "\n✅ Test completed successfully!"


-- | Preview testnet relay peer
-- Creates a test peer with dummy values for testing
mkPreviewRelay :: IO Peer
mkPreviewRelay = do
    now <- getCurrentTime
    let id' = ID (fromMaybe (error "oi") $ UUID.fromString "2ea41d90-6357-4e4a-a99b-066fc271a691")
    -- Correct port from Preview testnet topology.json
    (ip, portNumber) <- resolvePeerAddress "preview-node.world.dev.cardano.org" 3001
    pure
        Peer
            { id = id'
            , address = PeerAddress (NodeIP ip) (fromIntegral portNumber)
            , firstDiscovered = now
            , lastSeen = now
            , lastConnected = Nothing
            , discoveredVia = "manual-test"
            }


-- | Test connection to Preview testnet
testConnection
    :: (Conc :> es, IOE :> es, Log :> es, Network :> es, Sub :> es)
    => Eff es ()
testConnection = do
    previewRelay <- liftIO mkPreviewRelay
    Log.info $ "Connecting to " <> show previewRelay.address.host <> ":" <> show previewRelay.address.port

    -- Start event listeners in background
    Conc.fork_ networkEventListener
    Conc.fork_ peerSharingEventListener
    Conc.fork_ chainSyncEventListener

    -- Connect to peer
    conn <- connectToPeer previewRelay.address

    Log.info "✓ Connection established!"

    -- Check connection status
    isAlive <- isConnected conn
    if isAlive
        then Log.info "✓ Connection is alive"
        else Log.info "✗ Connection appears dead"

    -- Keep connection alive for 60 seconds to receive headers
    Log.info "Keeping connection alive for 60 seconds to receive headers..."
    liftIO $ threadDelay (60 * 1000000)

    -- Check status again
    isAlive' <- isConnected conn
    if isAlive'
        then Log.info "✓ Connection still alive after 30 seconds"
        else Log.info "✗ Connection died"


-- | Listen for and print network events
networkEventListener
    :: (IOE :> es, Log :> es, Sub :> es)
    => Eff es Void
networkEventListener = listen $ \event -> do
    case event of
        ConnectionEstablished dat -> do
            Log.info $ "🔗 Connection established with peer at " <> show dat.timestamp
        ConnectionLost dat -> do
            Log.info $ "💔 Connection lost: " <> dat.reason <> " at " <> show dat.timestamp
        HandshakeCompleted dat -> do
            Log.info $ "🤝 Handshake completed with version " <> show dat.version
        ProtocolError dat -> do
            Log.warn $ "❌ Protocol error: " <> dat.errorMessage
    liftIO $ hFlush stdout


-- | Listen for and print peer sharing events
peerSharingEventListener
    :: (IOE :> es, Log :> es, Sub :> es)
    => Eff es Void
peerSharingEventListener = listen $ \event -> do
    case event of
        PeerSharingStarted dat -> do
            Log.info $ "🔍 PeerSharing protocol started at " <> show dat.timestamp
        PeersReceived dat -> do
            Log.info $ "📡 Received " <> show (length dat.peerAddresses) <> " peer addresses from remote peer:"
            forM_ dat.peerAddresses $ \addr ->
                Log.debug $ "   - " <> show addr.host <> ":" <> show addr.port
        PeerSharingFailed dat -> do
            Log.warn $ "❌ PeerSharing failed: " <> dat.errorMessage
    liftIO $ hFlush stdout


-- | Listen for and print chain sync events
chainSyncEventListener
    :: (IOE :> es, Log :> es, Sub :> es)
    => Eff es Void
chainSyncEventListener = listen $ \event -> do
    case event of
        ChainSyncStarted dat -> do
            Log.info $ "⛓️  ChainSync protocol started at " <> show dat.timestamp
        HeaderReceived _dat -> do
            Log.info "📦 Header received!"
        RollBackward _dat -> do
            Log.info "⏪ Rollback occurred"
        RollForward _dat -> do
            Log.info "⏩ RollForward occurred"
        ChainSyncIntersectionFound _dat -> do
            Log.info "🎯 ChainSync intersection found"
    liftIO $ hFlush stdout


resolvePeerAddress :: Text -> Int -> IO (IP, PortNumber)
resolvePeerAddress address port = do
    let hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}
    addrs <- Socket.getAddrInfo (Just hints) (Just $ toString address) (Just $ show port)
    case addrs of
        (addr :| _) ->
            maybe (error "Found address of preview relay is not an IP address") pure $
                IP.fromSockAddr $
                    Socket.addrAddress addr
