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
import Prelude hiding (State, evalState)

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

import Cardano.Api
    ( ConsensusModeParams (CardanoModeParams)
    , EpochSlots (EpochSlots)
    , File (File)
    , LocalNodeConnectInfo (..)
    , NetworkId (Testnet)
    , NetworkMagic (NetworkMagic)
    )
import Data.Default (def)
import Effectful.Exception (tryIf)
import Effectful.State.Static.Shared (State, evalState)
import Hoard.Collector (dispatchDiscoveredNodes)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToClient (queryImmutableTip, runNodeToClient)
import Hoard.Effects.Pub (Pub)
import Hoard.Events.Collector (CollectorEvent (..))
import Hoard.Types.HoardState (HoardState)
import System.IO.Error (isDoesNotExistError)


connectionInfo :: LocalNodeConnectInfo
connectionInfo =
    ( LocalNodeConnectInfo
        -- according to
        -- https://cardano-api.cardano.intersectmbo.org/cardano-api/Cardano-Api-Network-IPC.html#g:2,
        -- https://book.world.dev.cardano.org/environments/preprod/shelley-genesis.json,
        -- https://book.world.dev.cardano.org/environments/mainnet/shelley-genesis.json,
        -- https://github.com/IntersectMBO/cardano-node/blob/master/configuration/cardano/mainnet-shelley-genesis.json#L62,
        -- and https://github.com/IntersectMBO/cardano-node/blob/master/nix/workbench/profile/presets/mainnet/genesis/genesis-shelley.json#L62
        (CardanoModeParams $ EpochSlots $ 432000)
        (Testnet $ NetworkMagic $ 1)
        (File "/home/rednaz/tweag/preprod/.run/preprod/cardano-node/node.socket")
    )


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
                . evalState @HoardState def
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
    :: ( Conc :> es
       , IOE :> es
       , Log :> es
       , Network :> es
       , Sub :> es
       , Pub :> es
       , State HoardState :> es
       )
    => Eff es ()
testConnection = do
    previewRelay <- liftIO mkPreviewRelay
    Log.info $ "Connecting to " <> show previewRelay.address.host <> ":" <> show previewRelay.address.port

    -- Start event listeners in background
    Conc.fork_ networkEventListener
    Conc.fork_ peerSharingEventListener
    Conc.fork_ chainSyncEventListener
    Conc.fork_ collectorEventListener
    Conc.fork_ dispatchDiscoveredNodes

    -- query immutable tip
    (=<<) (either (Log.warn . toText . displayException) pure)
        . tryIf isDoesNotExistError
        . runNodeToClient connectionInfo
        $ do
            Log.debug =<< show <$> queryImmutableTip
            liftIO $ threadDelay (5 * 1000000)
            Log.debug =<< show <$> queryImmutableTip

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
    :: (Log :> es, Sub :> es)
    => Eff es Void
networkEventListener = listen $ \case
    ConnectionEstablished dat -> do
        Log.info $ "🔗 Connection established with peer at " <> show dat.timestamp
    ConnectionLost dat -> do
        Log.info $ "💔 Connection lost: " <> dat.reason <> " at " <> show dat.timestamp
    HandshakeCompleted dat -> do
        Log.info $ "🤝 Handshake completed with version " <> show dat.version
    ProtocolError dat -> do
        Log.warn $ "❌ Protocol error: " <> dat.errorMessage


-- | Listen for and print peer sharing events
peerSharingEventListener
    :: (Log :> es, Sub :> es)
    => Eff es Void
peerSharingEventListener = listen $ \case
    PeerSharingStarted dat -> do
        Log.info $ "🔍 PeerSharing protocol started at " <> show dat.timestamp
    PeersReceived dat -> do
        Log.info $ "📡 Received " <> show (length dat.peerAddresses) <> " peer addresses from remote peer:"
        forM_ dat.peerAddresses $ \addr ->
            Log.debug $ "   - " <> show addr.host <> ":" <> show addr.port
    PeerSharingFailed dat -> do
        Log.warn $ "❌ PeerSharing failed: " <> dat.errorMessage


-- | Listen for and print chain sync events
chainSyncEventListener
    :: (Log :> es, Sub :> es)
    => Eff es Void
chainSyncEventListener = listen $ \case
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


collectorEventListener :: (Log :> es, Sub :> es) => Eff es Void
collectorEventListener = listen $ \case
    CollectorStarted addr -> Log.info $ "Collector: started for " <> show addr.host
    ConnectingToPeer addr -> Log.info $ "Collector: connecting to peer " <> show addr.host
    ConnectedToPeer addr -> Log.info $ "Collector: connected to peer " <> show addr.host
    ConnectionFailed addr reason -> Log.info $ "Collector: failed to connect to peer " <> show addr.host <> ": " <> reason
    ChainSyncReceived addr -> Log.info $ "Collector: chain sync received from " <> show addr.host
    BlockFetchReceived addr -> Log.info $ "Collector: block fetch received from " <> show addr.host


resolvePeerAddress :: Text -> Int -> IO (IP, PortNumber)
resolvePeerAddress address port = do
    let hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}
    addrs <- Socket.getAddrInfo (Just hints) (Just $ toString address) (Just $ show port)
    case addrs of
        (addr :| _) ->
            maybe (error "Found address of preview relay is not an IP address") pure $
                IP.fromSockAddr $
                    Socket.addrAddress addr
