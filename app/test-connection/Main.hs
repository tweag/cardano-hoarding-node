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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Void (Void)
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Ouroboros.Network.IOManager (withIOManager)
import System.IO (hFlush, stdout)

import Data.Text qualified as T
import Data.UUID qualified as UUID

import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Conc (Conc, scoped)
import Hoard.Effects.Network (Network, connectToPeer, isConnected, runNetwork)
import Hoard.Effects.Pub (runPub)
import Hoard.Effects.Sub (Sub, listen, runSub)
import Hoard.Network.Events

import Hoard.Effects.Conc qualified as Conc


main :: IO ()
main = withIOManager $ \ioManager -> do
    putStrLn "=== Cardano Peer Connection Test ==="
    putStrLn ""
    putStrLn "This program will:"
    putStrLn "  1. Connect to a Preview testnet relay"
    putStrLn "  2. Verify handshake completion"
    putStrLn "  3. Request peer addresses via PeerSharing protocol"
    putStrLn "  4. Receive headers via ChainSync protocol"
    putStrLn "  5. Keep the connection alive for 60 seconds"
    putStrLn ""

    -- Create event channel
    (inChan, _outChan) <- newChan

    -- Run the test
    result <- runEff
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
            putStrLn $ "\n❌ Test failed with error: " <> T.unpack err
        Right () -> do
            putStrLn "\n✅ Test completed successfully!"


-- | Preview testnet relay peer
-- Creates a test peer with dummy values for testing
mkPreviewRelay :: IO Peer
mkPreviewRelay = do
    now <- getCurrentTime
    let id' = ID (fromMaybe (error "oi") $ UUID.fromString "2ea41d90-6357-4e4a-a99b-066fc271a691")
    pure
        Peer
            { id = id'
            , address = "preview-node.world.dev.cardano.org"
            , port = 3001 -- Correct port from Preview testnet topology.json
            , firstDiscovered = now
            , lastSeen = now
            , discoveredVia = "manual-test"
            }


-- | Test connection to Preview testnet
testConnection
    :: (Conc :> es, IOE :> es, Network :> es, Sub :> es)
    => Eff es ()
testConnection = do
    previewRelay <- liftIO mkPreviewRelay
    liftIO $ putStrLn $ "Connecting to " <> T.unpack (address previewRelay) <> ":" <> show (port previewRelay)
    liftIO $ hFlush stdout

    -- Start event listeners in background
    Conc.fork_ networkEventListener
    Conc.fork_ peerSharingEventListener
    Conc.fork_ chainSyncEventListener

    -- Connect to peer
    conn <- connectToPeer previewRelay

    liftIO $ putStrLn "✓ Connection established!"

    -- Check connection status
    isAlive <- isConnected conn
    if isAlive
        then liftIO $ putStrLn "✓ Connection is alive"
        else liftIO $ putStrLn "✗ Connection appears dead"

    -- Keep connection alive for 60 seconds to receive headers
    liftIO $ putStrLn "Keeping connection alive for 60 seconds to receive headers..."
    liftIO $ threadDelay (60 * 1000000)

    -- Check status again
    isAlive' <- isConnected conn
    if isAlive'
        then liftIO $ putStrLn "✓ Connection still alive after 30 seconds"
        else liftIO $ putStrLn "✗ Connection died"


-- | Listen for and print network events
networkEventListener
    :: (IOE :> es, Sub :> es)
    => Eff es Void
networkEventListener = listen $ \event -> do
    liftIO $ case event of
        ConnectionEstablished dat -> do
            putStrLn $ "🔗 Connection established with peer at " <> show dat.timestamp
        ConnectionLost dat -> do
            putStrLn $ "💔 Connection lost: " <> T.unpack dat.reason <> " at " <> show dat.timestamp
        HandshakeCompleted dat -> do
            putStrLn $ "🤝 Handshake completed with version " <> show dat.version
        ProtocolError dat -> do
            putStrLn $ "❌ Protocol error: " <> T.unpack dat.errorMessage
    liftIO $ hFlush stdout


-- | Listen for and print peer sharing events
peerSharingEventListener
    :: (IOE :> es, Sub :> es)
    => Eff es Void
peerSharingEventListener = listen $ \event -> do
    liftIO $ case event of
        PeerSharingStarted dat -> do
            putStrLn $ "🔍 PeerSharing protocol started at " <> show dat.timestamp
        PeersReceived dat -> do
            putStrLn $ "📡 Received " <> show dat.peerCount <> " peer addresses from remote peer:"
            mapM_ (\addr -> putStrLn $ "   - " <> T.unpack addr) dat.peerAddresses
        PeerSharingFailed dat -> do
            putStrLn $ "❌ PeerSharing failed: " <> T.unpack dat.errorMessage
    liftIO $ hFlush stdout


-- | Listen for and print chain sync events
chainSyncEventListener
    :: (IOE :> es, Sub :> es)
    => Eff es Void
chainSyncEventListener = listen $ \event -> do
    liftIO $ case event of
        ChainSyncStarted dat -> do
            putStrLn $ "⛓️  ChainSync protocol started at " <> show dat.timestamp
        HeaderReceived _dat -> do
            putStrLn "📦 Header received!"
        RollBackward _dat -> do
            putStrLn "⏪ Rollback occurred"
        RollForward _dat -> do
            putStrLn "⏩ RollForward occurred"
        ChainSyncIntersectionFound _dat -> do
            putStrLn "🎯 ChainSync intersection found"
    liftIO $ hFlush stdout
