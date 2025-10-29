{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Hoard.Effects.Network
-- Description: Network effect for managing peer connections
--
-- This effect provides high-level operations for connecting to Cardano peers
-- and managing the node-to-node protocol communication.
module Hoard.Effects.Network
    ( -- * Effect
      Network
    , connectToPeer
    , disconnectPeer
    , isConnected

      -- * Interpreter
    , runNetwork
    ) where

import Codec.CBOR.Encoding (Encoding, encodeString)
import Control.Concurrent.Chan.Unagi (InChan, writeChan)
import Control.Tracer (nullTracer)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import Network.Mux (Mode (..), StartOnDemandOrEagerly (..))
import Network.Socket (AddrInfo (..), SockAddr, SocketType (Stream))
import Ouroboros.Network.Diffusion.Configuration (PeerSharing (..))
import Ouroboros.Network.Driver.Simple (runPeer)
import Ouroboros.Network.IOManager (IOManager, withIOManager)
import Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..), MiniProtocolLimits (..), OuroborosApplication (..), OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..))
import Ouroboros.Network.NodeToNode
    ( DiffusionMode (..)
    , MiniProtocolParameters (..)
    , NodeToNodeVersion (..)
    , NodeToNodeVersionData (..)
    , blockFetchMiniProtocolNum
    , chainSyncMiniProtocolNum
    , combineVersions
    , connectTo
    , defaultMiniProtocolParameters
    , keepAliveMiniProtocolNum
    , nullNetworkConnectTracers
    , peerSharingMiniProtocolNum
    , simpleSingletonVersions
    , txSubmissionMiniProtocolNum
    )
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient (..), peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Codec (codecPeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))
import Ouroboros.Network.Snocket (socketSnocket)

import Codec.CBOR.Decoding qualified as CBOR
import Data.ByteString.Lazy qualified as LBS
import Data.Dynamic qualified as Dyn
import Data.Text qualified as T
import Debug.Trace qualified
import Network.Socket qualified as Socket

import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Network.Config (NetworkConfig (..))
import Hoard.Network.Events
    ( ConnectionEstablishedData (..)
    , ConnectionLostData (..)
    , HandshakeCompletedData (..)
    , NetworkEvent (..)
    , PeerSharingEvent (..)
    , PeerSharingStartedData (..)
    , PeersReceivedData (..)
    )
import Hoard.Network.Types (Connection (..))


--------------------------------------------------------------------------------
-- Network Effect
--------------------------------------------------------------------------------

-- | Effect for managing peer connections.
--
-- Provides operations to connect to peers, disconnect, and check connection status.
data Network :: Effect where
    ConnectToPeer :: NetworkConfig -> Peer -> Network m Connection
    DisconnectPeer :: Connection -> Network m ()
    IsConnected :: Connection -> Network m Bool


-- Generate smart constructors using Template Haskell
makeEffect ''Network


--------------------------------------------------------------------------------
-- Effect Handler
--------------------------------------------------------------------------------

-- | Run the Network effect with real implementation.
--
-- This handler establishes actual network connections and spawns protocol threads.
-- Requires IOE, Pub, Conc, and Error effects in the stack.
runNetwork
    :: (Error Text :> es, IOE :> es, Pub :> es)
    => InChan Dyn.Dynamic
    -> Eff (Network : es) a
    -> Eff es a
runNetwork chan = interpret $ \_ -> \case
    ConnectToPeer config peer -> connectToPeerImpl chan config peer
    DisconnectPeer conn -> disconnectPeerImpl chan conn
    IsConnected conn -> isConnectedImpl conn


--------------------------------------------------------------------------------
-- Implementation Functions
--------------------------------------------------------------------------------

-- | Implementation of connectToPeer.
--
-- This implementation:
-- 1. Resolves the peer's address
-- 2. Creates an IOManager and Snocket
-- 3. Constructs version negotiation data
-- 4. Creates mini-protocol applications
-- 5. Connects using ouroboros-network's connectTo
-- 6. Publishes connection events
connectToPeerImpl
    :: (Error Text :> es, IOE :> es, Pub :> es)
    => InChan Dyn.Dynamic
    -> NetworkConfig
    -> Peer
    -> Eff es Connection
connectToPeerImpl chan config peer = do
    -- Resolve address
    liftIO $ putStrLn "[DEBUG] Resolving peer address..."
    addr <- liftIO $ resolvePeerAddress peer
    liftIO $ putStrLn $ "[DEBUG] Resolved to: " <> show addr

    -- Create connection using ouroboros-network
    liftIO $ putStrLn "[DEBUG] Creating IOManager and attempting connection..."

    -- Create a publish callback that can be called from IO
    let publishIO :: forall event. (Typeable event) => event -> IO ()
        publishIO event = writeChan chan (Dyn.toDyn event)

    result <- liftIO $ withIOManager $ \ioManager -> do
        liftIO $ putStrLn "[DEBUG] IOManager created, creating snocket..."
        let snocket = socketSnocket ioManager

        -- Create version data for handshake
        let versionData =
                NodeToNodeVersionData
                    { networkMagic = config.networkMagic
                    , diffusionMode = InitiatorOnlyDiffusionMode
                    , peerSharing = PeerSharingEnabled -- Enable peer sharing
                    , query = False
                    }

        -- Create versions for negotiation - offer both V_14 and V_15
        -- to increase compatibility with different node versions
        liftIO $ putStrLn "[DEBUG] Creating protocol versions..."
        let versionsV14 =
                simpleSingletonVersions
                    NodeToNodeV_14
                    versionData
                    (\_ -> mkApplication ioManager peer publishIO)
        let versionsV15 =
                simpleSingletonVersions
                    NodeToNodeV_15
                    versionData
                    (\_ -> mkApplication ioManager peer publishIO)
        let versions = combineVersions [versionsV14, versionsV15]

        -- Connect to the peer
        liftIO $ putStrLn "[DEBUG] Calling connectTo..."
        result <-
            connectTo
                snocket
                nullNetworkConnectTracers
                versions
                Nothing -- No local address binding
                addr
        liftIO $ putStrLn "[DEBUG] connectTo returned!"
        pure result

    case result of
        Left err -> do
            throwError $ "Failed to connect to peer " <> T.pack (show peer) <> ": " <> T.pack (show err)
        Right (Left ()) -> do
            -- Connection succeeded, create Connection record
            timestamp <- liftIO getCurrentTime
            let version = NodeToNodeV_14 -- We negotiated this version

            -- Publish handshake completed event
            publish $ HandshakeCompleted HandshakeCompletedData {peer, version, timestamp}

            -- Note: The mini-protocols are already running in the background
            -- via the application we passed to connectTo

            -- Create connection record
            let conn =
                    Connection
                        { peer = peer
                        , version = version
                        , started = timestamp
                        }

            -- Publish connection established event
            publish $ ConnectionEstablished ConnectionEstablishedData {peer, version, timestamp}

            pure conn
        Right (Right _) -> do
            -- This shouldn't happen with InitiatorOnly mode
            throwError ("Unexpected responder mode result" :: Text)


-- | Implementation of disconnectPeer.
--
-- Note: With the current connectTo-based implementation, we don't have direct control
-- over disconnection. The connection is managed by ouroboros-network's internal state.
disconnectPeerImpl
    :: (IOE :> es, Pub :> es)
    => InChan Dyn.Dynamic
    -> Connection
    -> Eff es ()
disconnectPeerImpl _chan conn = do
    -- Publish connection lost event
    timestamp <- liftIO getCurrentTime
    let peer = Hoard.Network.Types.peer conn
        reason = "Disconnect requested"
    publish $ ConnectionLost ConnectionLostData {peer, reason, timestamp}


-- Note: Actual socket closing is handled by ouroboros-network

-- | Implementation of isConnected.
--
-- Note: With the current implementation, we can't easily check connection status
-- since it's managed internally by ouroboros-network.
isConnectedImpl
    :: Connection
    -> Eff es Bool
isConnectedImpl _conn = do
    -- For now, we'll assume connections are persistent
    -- In a full implementation, we'd track connection state
    pure True


--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Resolve peer address to socket address.
resolvePeerAddress :: Peer -> IO SockAddr
resolvePeerAddress peer = do
    let hints = Socket.defaultHints {addrSocketType = Stream}
    addrs <- Socket.getAddrInfo (Just hints) (Just $ T.unpack $ address peer) (Just $ show $ port peer)
    case addrs of
        [] -> Prelude.error $ "Could not resolve address: " <> show peer
        (addr : _) -> pure $ addrAddress addr


--------------------------------------------------------------------------------
-- Mini-Protocol Application
--------------------------------------------------------------------------------

-- | Create the Ouroboros application with all mini-protocols.
--
-- This bundles together ChainSync, BlockFetch, and KeepAlive protocols into
-- an application that runs over the multiplexed connection.
--
-- The publishEvent callback allows protocols to publish events back to the
-- application's event bus.
mkApplication
    :: IOManager
    -> Peer
    -> (forall event. (Typeable event) => event -> IO ())
    -- ^ Publish event callback
    -> OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void
mkApplication _ioManager peer publishEvent =
    OuroborosApplication
        [ -- ChainSync mini-protocol (stub)
          MiniProtocol
            { miniProtocolNum = chainSyncMiniProtocolNum
            , miniProtocolLimits = chainSyncLimits
            , miniProtocolStart = StartOnDemand
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately to allow connection to complete
                -- Real implementation in Ticket #3 will do actual protocol handshake
                putStrLn "[DEBUG] ChainSync protocol stub started"
                pure ((), Nothing)
            }
        , -- BlockFetch mini-protocol (stub)
          MiniProtocol
            { miniProtocolNum = blockFetchMiniProtocolNum
            , miniProtocolLimits = blockFetchLimits
            , miniProtocolStart = StartOnDemand
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] BlockFetch protocol stub started"
                pure ((), Nothing)
            }
        , -- KeepAlive mini-protocol (stub)
          MiniProtocol
            { miniProtocolNum = keepAliveMiniProtocolNum
            , miniProtocolLimits = keepAliveLimits
            , miniProtocolStart = StartOnDemand
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] KeepAlive protocol stub started"
                pure ((), Nothing)
            }
        , -- PeerSharing mini-protocol
          MiniProtocol
            { miniProtocolNum = peerSharingMiniProtocolNum
            , miniProtocolLimits = peerSharingLimits
            , miniProtocolStart = StartEagerly
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx channel -> do
                -- Publish started event
                timestamp <- getCurrentTime
                publishEvent $ PeerSharingStarted PeerSharingStartedData {peer, timestamp}
                putStrLn "[DEBUG] PeerSharing: Published PeerSharingStarted event"

                -- Create and run the PeerSharing client
                let client = peerSharingClientImpl peer publishEvent
                let clientPeer = peerSharingClientPeer client
                    codec = codecPeerSharing encodeSockAddr decodeSockAddr

                putStrLn "[DEBUG] PeerSharing: About to run peer protocol..."
                -- Run the protocol
                _ <- runPeer nullTracer codec channel clientPeer
                putStrLn "[DEBUG] PeerSharing: Protocol completed"
                pure ((), Nothing)
            }
        , -- TxSubmission mini-protocol (stub, not needed for hoarding)
          MiniProtocol
            { miniProtocolNum = txSubmissionMiniProtocolNum
            , miniProtocolLimits = txSubmissionLimits
            , miniProtocolStart = StartOnDemand
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] TxSubmission protocol stub started"
                pure ((), Nothing)
            }
        ]
  where
    -- Get protocol parameters
    params = defaultMiniProtocolParameters

    -- Protocol limits from parameters
    -- Using chainSyncPipeliningHighMark as a reasonable default for all protocols
    chainSyncLimits =
        MiniProtocolLimits
            { maximumIngressQueue = fromIntegral $ chainSyncPipeliningHighMark params
            }
    blockFetchLimits =
        MiniProtocolLimits
            { maximumIngressQueue = fromIntegral $ blockFetchPipeliningMax params
            }
    keepAliveLimits =
        MiniProtocolLimits
            { maximumIngressQueue = 1000 -- Reasonable default for keep alive
            }
    peerSharingLimits =
        MiniProtocolLimits
            { maximumIngressQueue = 1000 -- Reasonable default for peer sharing
            }
    txSubmissionLimits =
        MiniProtocolLimits
            { maximumIngressQueue = fromIntegral $ txSubmissionMaxUnacked params
            }


--------------------------------------------------------------------------------
-- PeerSharing Protocol Implementation
--------------------------------------------------------------------------------

-- | Create a PeerSharing client that requests peer addresses.
--
-- This client:
-- 1. Requests up to 100 peer addresses from the remote peer
-- 2. Publishes a PeersReceived event with the results
-- 3. Terminates after one request
peerSharingClientImpl
    :: Peer
    -> (forall event. (Typeable event) => event -> IO ())
    -> PeerSharingClient SockAddr IO ()
peerSharingClientImpl peer publishEvent =
    Debug.Trace.trace "[DEBUG] PeerSharing: Creating SendMsgShareRequest..." $
        SendMsgShareRequest (PeerSharingAmount 100) $ \peerAddrs -> do
            putStrLn "[DEBUG] PeerSharing: *** CALLBACK EXECUTED - GOT RESPONSE ***"
            putStrLn $ "[DEBUG] PeerSharing: Received response with " <> show (length peerAddrs) <> " peers"
            timestamp <- getCurrentTime
            let peerAddrTexts = map (T.pack . show) peerAddrs
                peerCount = length peerAddrs
            publishEvent $
                PeersReceived
                    PeersReceivedData
                        { peer = peer
                        , peerAddresses = peerAddrTexts
                        , peerCount = peerCount
                        , timestamp = timestamp
                        }
            putStrLn "[DEBUG] PeerSharing: Published PeersReceived event"
            pure $ SendMsgDone (pure ())


-- | Encode SockAddr to CBOR (simple string representation for now)
encodeSockAddr :: SockAddr -> Encoding
encodeSockAddr addr =
    Debug.Trace.trace ("[DEBUG] encodeSockAddr: encoding " <> show addr) $
        encodeString $
            T.pack $
                show addr


-- | Decode SockAddr from CBOR (simple string parsing for now)
decodeSockAddr :: forall s. CBOR.Decoder s SockAddr
decodeSockAddr = do
    -- Peek at what we're about to decode
    tokenType <- Debug.Trace.trace "[DEBUG] decodeSockAddr: About to decode SockAddr" CBOR.peekTokenType
    Debug.Trace.trace ("[DEBUG] decodeSockAddr: Token type = " <> show tokenType) (pure ())

    -- For now, just read and parse the string representation
    -- In production, we'd want proper CBOR encoding of the address structure
    str <- CBOR.decodeString
    Debug.Trace.trace ("[DEBUG] decodeSockAddr: Decoded string = " <> show str) (pure ())

    -- This is a simplified placeholder - in reality we'd properly parse the address
    pure $ Socket.SockAddrInet 0 0 -- Placeholder
