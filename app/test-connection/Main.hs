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
import Hoard.Types.NodeIP (NodeIP (..))

import Data.Default (def)
import Effectful.Exception (tryIf)
import Effectful.State.Static.Shared (State, evalState)
import Hoard.Collector (dispatchDiscoveredNodes)
import Hoard.Effects.Clock (runClock)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToClient (immutableTip, isOnChain, runNodeToClient)
import Hoard.Effects.Pub (Pub)
import Hoard.Listeners.ChainSyncEventListener (chainSyncEventListener)
import Hoard.Listeners.CollectorEventListener (collectorEventListener)
import Hoard.Listeners.NetworkEventListener (networkEventListener)
import Hoard.Listeners.PeerSharingEventListener (peerSharingEventListener)
import Hoard.Types.HoardState (HoardState)
import System.IO.Error (isDoesNotExistError)


connectionConfig :: ConnectionConfig
connectionConfig = ConnectionConfig {protocolConfigPath = "config/preview/config.json", localNodeSocketPath = "preview.socket"}


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
        . runClock
        . scoped
        $ \scope -> do
            Conc.runConcWithKi scope
                . runErrorNoCallStack @Text
                . runSub inChan
                . runPub inChan
                . runNetwork ioManager "config/preview/config.json"
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
    Conc.fork_ $ listen networkEventListener
    Conc.fork_ $ listen peerSharingEventListener
    Conc.fork_ $ listen chainSyncEventListener
    Conc.fork_ $ listen collectorEventListener
    Conc.fork_ $ listen dispatchDiscoveredNodes

    -- query immutable tip
    (=<<) (either (Log.warn . toText . displayException) pure)
        . tryIf isDoesNotExistError
        . (\action -> scoped (\scope -> Conc.runConcWithKi scope $ runNodeToClient connectionConfig action))
        $ do
            tip0 <- immutableTip
            Log.debug (show tip0)
            Log.debug =<< show <$> isOnChain tip0
            liftIO $ threadDelay (5 * 1000000)
            tip1 <- immutableTip
            Log.debug (show tip1)
            Log.debug =<< show <$> isOnChain tip1

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


resolvePeerAddress :: Text -> Int -> IO (IP, PortNumber)
resolvePeerAddress address port = do
    let hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}
    addrs <- Socket.getAddrInfo (Just hints) (Just $ toString address) (Just $ show port)
    case addrs of
        (addr :| _) ->
            maybe (error "Found address of preview relay is not an IP address") pure $
                IP.fromSockAddr $
                    Socket.addrAddress addr


data ConnectionConfig = ConnectionConfig {protocolConfigPath :: FilePath, localNodeSocketPath :: FilePath}
