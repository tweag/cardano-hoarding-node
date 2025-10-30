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

import Codec.CBOR.Decoding (Decoder, decodeListLen, decodeWord, decodeWord16)
import Codec.CBOR.Encoding (Encoding, encodeListLen, encodeWord, encodeWord16)
import Codec.Serialise (Serialise (decode, encode), deserialise, serialise)
import Control.Monad (when)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Void (Void)
import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Network.Config (NetworkConfig (..))
import Hoard.Network.Events
    ( ConnectionEstablishedData (..)
    , ConnectionLostData (..)
    , HandshakeCompletedData (..)
    , NetworkEvent (..)
    )
import Hoard.Network.Types (Connection (..))
import Network.Mux (Mode (..), StartOnDemandOrEagerly (..))
import Network.Socket (AddrInfo (..), SockAddr (SockAddrInet, SockAddrInet6, SockAddrUnix), SocketType (Stream))
import Ouroboros.Network.Diffusion.Configuration (PeerSharing (..))
import Ouroboros.Network.IOManager (IOManager, withIOManager)
import Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..), MiniProtocolLimits (..), OuroborosApplication (..), OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..))
import Ouroboros.Network.NodeToNode
    ( DiffusionMode (..)
    , NodeToNodeVersion (..)
    , NodeToNodeVersionData (..)
    , combineVersions
    , connectTo
    , keepAliveMiniProtocolNum
    , nullNetworkConnectTracers
    , simpleSingletonVersions
    )
import Ouroboros.Network.Snocket (socketSnocket)

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.Socket qualified as Socket


--------------------------------------------------------------------------------
-- Network Effect
--------------------------------------------------------------------------------

-- | Effect for managing peer connections.
--
-- Provides operations to connect to peers, disconnect, and check connection status.
data Network :: Effect where
    ConnectToPeer :: Peer -> Network m Connection
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
    => NetworkConfig
    -> Eff (Network : es) a
    -> Eff es a
runNetwork config = interpret $ \_ -> \case
    ConnectToPeer peer -> connectToPeerImpl config peer
    DisconnectPeer conn -> disconnectPeerImpl conn
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
    => NetworkConfig
    -> Peer
    -> Eff es Connection
connectToPeerImpl config peer = do
    -- Resolve address
    liftIO $ putStrLn "[DEBUG] Resolving peer address..."
    addr <- liftIO $ resolvePeerAddress peer
    liftIO $ putStrLn $ "[DEBUG] Resolved to: " <> show addr

    -- Create connection using ouroboros-network
    liftIO $ putStrLn "[DEBUG] Creating IOManager and attempting connection..."
    result <- liftIO $ withIOManager $ \ioManager -> do
        liftIO $ putStrLn "[DEBUG] IOManager created, creating snocket..."
        let snocket = socketSnocket ioManager

        -- Create version data for handshake
        let versionData =
                NodeToNodeVersionData
                    { networkMagic = config.networkMagic
                    , diffusionMode = InitiatorOnlyDiffusionMode
                    , peerSharing = PeerSharingDisabled
                    , query = False
                    }

        -- Create versions for negotiation - offer both V_14 and V_15
        -- to increase compatibility with different node versions
        liftIO $ putStrLn "[DEBUG] Creating protocol versions..."
        let versionsV14 =
                simpleSingletonVersions
                    NodeToNodeV_14
                    versionData
                    (\_ -> mkApplication ioManager peer)
        let versionsV15 =
                simpleSingletonVersions
                    NodeToNodeV_15
                    versionData
                    (\_ -> mkApplication ioManager peer)
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
    => Connection
    -> Eff es ()
disconnectPeerImpl conn = do
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
-- For Ticket #1, these are minimal stubs that just keep the connection alive.
-- Proper protocol implementations will be added in later tickets.
mkApplication
    :: IOManager
    -> Peer
    -> OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void
mkApplication _ioManager _peer =
    OuroborosApplication
        [ -- KeepAlive mini-protocol (stub)
          MiniProtocol
            { miniProtocolNum = keepAliveMiniProtocolNum
            , miniProtocolLimits = keepAliveLimits
            , miniProtocolStart = StartOnDemand
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] KeepAlive protocol stub started"
                pure ((), Nothing)
            }
        ]
  where
    keepAliveLimits =
        MiniProtocolLimits
            { maximumIngressQueue = 1000 -- Reasonable default for keep alive
            }


encodeSockAddr :: SockAddr -> Encoding
encodeSockAddr (SockAddrInet portNumber hostAddress) =
    encodeListLen 3
        <> encodeWord 0
        <> ( encodeListLen 2
                <> encodeWord 0
                <> encodeWord16 (fromIntegral portNumber)
           )
        <> encode hostAddress
encodeSockAddr (SockAddrInet6 portNumber flowInfo hostAddress6 scopeID) =
    encodeListLen 5
        <> encodeWord 1
        <> ( encodeListLen 2
                <> encodeWord 0
                <> encodeWord16 (fromIntegral portNumber)
           )
        <> encode flowInfo
        <> encode hostAddress6
        <> encode scopeID
encodeSockAddr (SockAddrUnix string) =
    encodeListLen 2
        <> encodeWord 2
        <> encode string


decodeSockAddr :: Decoder s SockAddr
decodeSockAddr =
    do
        len <- decodeListLen
        tag <- decodeWord
        case (len, tag) of
            (3, 0) -> do
                len' <- decodeListLen
                when (len' /= 2) $ fail "expect list of length 2"
                tag' <- decodeWord
                when (tag' /= 0) $ fail "unexpected tag. Expect 0"
                portNumber <- decodeWord16
                SockAddrInet (fromIntegral portNumber) <$> decode
            (5, 1) -> do
                len' <- decodeListLen
                when (len' /= 2) $ fail "expect list of length 2"
                tag' <- decodeWord
                when (tag' /= 0) $ fail "unexpected tag. Expect 0"
                portNumber <- decodeWord16
                SockAddrInet6 (fromIntegral portNumber) <$> decode <*> decode <*> decode
            (2, 2) -> do
                SockAddrUnix <$> decode
            _ -> fail "invalid SockAddr encoding"


-- tests

instance Serialise SockAddr where
    encode = encodeSockAddr
    decode = decodeSockAddr


test :: Bool
test =
    roundTripTest (SockAddrInet 8080 (Socket.tupleToHostAddress (127, 0, 0, 1)))
        && roundTripTest (SockAddrInet6 8080 0 (Socket.tupleToHostAddress6 (1, 2, 3, 4, 5, 6, 7, 8)) 0)
        && roundTripTest (SockAddrInet6 80 0 (0, 0, 0xffff, 0x01020304) 0)
        && roundTripTest (SockAddrUnix "/tmp/socket")
  where
    roundTripTest :: SockAddr -> Bool
    roundTripTest addr = (deserialise $ serialise $ addr) == addr

-- >>> test
-- True
