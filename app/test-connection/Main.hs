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
import Data.Default (def)
import Data.IP (IP)
import Data.IP qualified as IP
import Data.Set qualified as S
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Exception (tryIf)
import Effectful.State.Static.Shared (State, evalState)
import Network.Socket (PortNumber)
import Network.Socket qualified as Socket
import Options.Applicative qualified as Opt
import Ouroboros.Network.IOManager (withIOManager)
import System.IO.Error (isDoesNotExistError)
import Prelude hiding (State, evalState)

import Hoard.CLI.Options (Options (..), optsParser)
import Hoard.Collector (dispatchDiscoveredNodes)
import Hoard.Config.Loader (loadConfig)
import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects (Config (..))
import Hoard.Effects.Chan (Chan, runChan)
import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Clock (Clock, runClock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc (Conc, scoped)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.DBRead (runDBRead)
import Hoard.Effects.DBWrite (runDBWrite)
import Hoard.Effects.HeaderRepo (HeaderRepo, runHeaderRepo)
import Hoard.Effects.Input (input, runInputChan)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToClient (immutableTip, isOnChain, runNodeToClient)
import Hoard.Effects.NodeToNode (NodeToNode, connectToPeer, runNodeToNode)
import Hoard.Effects.NodeToNode qualified as NodeToNode
import Hoard.Effects.Output (output, runOutputChan)
import Hoard.Effects.PeerRepo (PeerRepo, runPeerRepo, upsertPeers)
import Hoard.Effects.Pub (Pub, publish, runPub)
import Hoard.Effects.Sub (Sub, listen, runSub)
import Hoard.Network.Events
    ( BlockFetchEvent (..)
    , BlockFetchRequest (..)
    , ChainSyncEvent (..)
    , HeaderReceivedData (..)
    )
import Hoard.Types.DBConfig (DBPools (..))
import Hoard.Types.Environment (Environment (..))
import Hoard.Types.NodeIP (NodeIP (..))

import Hoard.Listeners.ChainSyncEventListener (chainSyncEventListener)
import Hoard.Listeners.CollectorEventListener (collectorEventListener)
import Hoard.Listeners.NetworkEventListener (networkEventListener)
import Hoard.Listeners.PeerSharingEventListener (peerSharingEventListener)
import Hoard.Types.HoardState (HoardState)


connectionConfig :: ConnectionConfig
connectionConfig = ConnectionConfig {protocolConfigPath = "config/preview/config.json", localNodeSocketPath = "preview.socket"}


main :: IO ()
main = withIOManager $ \ioManager -> do
    options <- Opt.execParser optsParser

    -- Determine environment: CLI flag > default to Dev
    let env = fromMaybe Dev options.environment

    putTextLn $ "Loading configuration for environment: " <> show env
    config <- loadConfig ioManager env

    putTextLn "=== Cardano Peer Connection Test ==="
    putTextLn ""
    putTextLn "This program will:"
    putTextLn "  1. Connect to a Preview testnet relay"
    putTextLn "  2. Verify handshake completion"
    putTextLn "  3. Request peer addresses via PeerSharing protocol"
    putTextLn "  4. Receive headers via ChainSync protocol"
    putTextLn "  5. Keep the connection alive for 60 seconds"
    putTextLn ""

    -- Run the test
    result <- runEff
        . Log.runLog config.logging
        . runConcurrent
        . runClock
        . scoped
        $ \scope -> do
            Conc.runConcWithKi scope
                . runErrorNoCallStack @Text
                . runChan
                . runSub config.inChan
                . runPub config.inChan
                . runNodeToNode config.ioManager config.protocolConfigPath
                . runDBRead config.dbPools.readerPool
                . runDBWrite config.dbPools.writerPool
                . runPeerRepo
                . runHeaderRepo
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
    :: ( Chan :> es
       , Conc :> es
       , IOE :> es
       , Log :> es
       , NodeToNode :> es
       , Sub :> es
       , Pub :> es
       , State HoardState :> es
       , HeaderRepo :> es
       , PeerRepo :> es
       , Clock :> es
       )
    => Eff es ()
testConnection = do
    previewRelay <- liftIO mkPreviewRelay
    Log.info $ "Connecting to " <> show previewRelay.address.host <> ":" <> show previewRelay.address.port

    -- Upsert the peer to the database first (bootstrap: source is itself)
    -- upsertPeers returns the peer with DB-assigned ID
    timestamp <- Clock.currentTime
    upsertedPeers <- upsertPeers (S.singleton previewRelay.address) previewRelay.address timestamp
    peer <- case toList upsertedPeers of
        [p] -> pure p
        _ -> error "Expected exactly one peer from upsert"

    -- Start event listeners in background
    Conc.fork_ $ listen networkEventListener
    Conc.fork_ $ listen peerSharingEventListener
    Conc.fork_ $ listen chainSyncEventListener
    Conc.fork_ $ listen collectorEventListener
    Conc.fork_ $ listen dispatchDiscoveredNodes

    -- query immutable tip
    (either (Log.warn . toText . displayException) pure =<<)
        . tryIf isDoesNotExistError
        . (\eff -> scoped (\scope -> Conc.runConcWithKi scope $ runNodeToClient connectionConfig eff))
        $ do
            tip0 <- immutableTip
            Log.debug (show tip0)
            Log.debug =<< show <$> isOnChain tip0
            liftIO $ threadDelay (5 * 1000000)
            tip1 <- immutableTip
            Log.debug (show tip1)
            Log.debug =<< show <$> isOnChain tip1

    (inChan, outChan) <- Chan.newChan
    -- Connect to peer
    _ <- runInputChan outChan . runOutputChan inChan $ do
        Conc.fork $ listen \case
            HeaderReceived dat ->
                output $
                    BlockFetchRequest
                        { timestamp = dat.timestamp
                        , point = dat.point
                        , peer = dat.peer.address
                        }
            _ -> pure ()

        connectToPeer $
            NodeToNode.Config
                { peer
                , emitFetchedBlock = publish . BlockReceived
                , emitFetchedHeader = publish . HeaderReceived
                , awaitBlockFetchRequest = input
                }

    Log.info "Connection closed!"


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
