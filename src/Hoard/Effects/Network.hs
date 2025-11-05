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
    , connectToPeerImpl

      -- * Interpreter
    , runNetwork
    ) where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Chan.Unagi (InChan, writeChan)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable)
import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error)
import Effectful.TH (makeEffect)
import Ouroboros.Network.IOManager (IOManager)
import Ouroboros.Network.NodeToNode
    ( DiffusionMode (..)
    , NodeToNodeVersion (..)
    )
import Ouroboros.Network.Snocket (socketSnocket)

import Data.Dynamic qualified as Dyn
import Ouroboros.Network.ConnectionManager.Types qualified as CM

import Hoard.Data.Peer (Peer (..), resolvePeerAddress)
import Hoard.Effects.Network.ConnectionManager (withConnectionManager)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Network.Config (NetworkConfig (..))
import Hoard.Network.Events
    ( ConnectionEstablishedData (..)
    , ConnectionLostData (..)
    , NetworkEvent (..)
    )
import Hoard.Network.Types (Connection (..))


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
    => IOManager
    -> NetworkConfig
    -> InChan Dyn.Dynamic
    -> Eff (Network : es) a
    -> Eff es a
runNetwork ioManager config chan = interpret $ \_ -> \case
    ConnectToPeer peer -> undefined
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
    :: IOManager
    -> NetworkConfig
    -> InChan Dyn.Dynamic
    -> Peer
    -> IO ()
connectToPeerImpl ioManager config chan peer = do
    -- Resolve address
    liftIO $ putStrLn "[DEBUG] Resolving peer address..."
    addr <- liftIO $ resolvePeerAddress peer
    liftIO $ putStrLn $ "[DEBUG] Resolved to: " <> show addr

    -- Create connection using ouroboros-network
    liftIO $ putStrLn "[DEBUG] Attempting connection..."
    liftIO $ putStrLn "[DEBUG] Creating snocket..."
    let snocket = socketSnocket ioManager

    -- Create a publish callback that can be called from IO
    let publishIO :: forall event. (Typeable event) => event -> IO ()
        publishIO event = writeChan chan (Dyn.toDyn event)

    -- -- Create version data for handshake
    -- let versionData =
    --         NodeToNodeVersionData
    --             { networkMagic = config.networkMagic
    --             , diffusionMode = InitiatorOnlyDiffusionMode
    --             , peerSharing = PeerSharingDisabled
    --             , query = False
    --             }
    --
    -- -- Create versions for negotiation - offer both V_14 and V_15
    -- -- to increase compatibility with different node versions
    -- liftIO $ putStrLn "[DEBUG] Creating protocol versions..."
    -- let versionsV14 =
    --         simpleSingletonVersions
    --             NodeToNodeV_14
    --             versionData
    --             $ mkApplication' peer publishIO NodeToNodeV_14
    -- let versionsV15 =
    --         simpleSingletonVersions
    --             NodeToNodeV_15
    --             versionData
    --             $ mkApplication' peer publishIO NodeToNodeV_15
    -- let versions = combineVersions [versionsV14, versionsV15]
    --
    -- -- Connect to the peer
    -- liftIO $ putStrLn "[DEBUG] Calling connectTo..."
    -- result <-
    --     liftIO
    --         $ connectTo
    --             snocket
    --             nullNetworkConnectTracers
    --             versions
    --             Nothing -- No local address binding
    --             addr
    -- liftIO $ putStrLn "[DEBUG] connectTo returned!"
    liftIO $ putStrLn "[DEBUG] Starting connection manager..."
    threadId <- liftIO myThreadId
    liftIO $ withConnectionManager threadId config.networkMagic publishIO peer snocket $ \cm -> do
        putStrLn "[DEBUG] Connection manager started!"
        let connect = CM.acquireOutboundConnection cm
        putStrLn "[DEBUG] Connecting to peer..."
        connection <- connect InitiatorOnlyDiffusionMode addr
        case connection of
            CM.Connected _connId _dataFlow _handle -> do
                putStrLn "[DEBUG] Connected!"
                timestamp <- getCurrentTime
                publishIO
                    $ ConnectionEstablished
                    $ ConnectionEstablishedData
                        { peer
                        , timestamp
                        , -- TODO: Get the actual version for the connected
                          -- node. Or do we even need to note down the version
                          -- here at all?
                          version = NodeToNodeV_14
                        }
            CM.Disconnected _connId maybeError -> do
                putStrLn "[DEBUG] Connection failed!"
                putStrLn $ "Reason: " <> show maybeError

        threadDelay 30_000_000


-- case result of
--     Left err -> do
--         throwError $ "Failed to connect to peer " <> T.pack (show peer) <> ": " <> T.pack (show err)
--     Right (Left ()) -> do
--         -- Connection succeeded, create Connection record
--         timestamp <- liftIO getCurrentTime
--         let version = NodeToNodeV_14 -- We negotiated this version
--
--         -- Publish handshake completed event
--         publish $ HandshakeCompleted HandshakeCompletedData {peer, version, timestamp}
--
--         -- Note: The mini-protocols are already running in the background
--         -- via the application we passed to connectTo
--
--         -- Create connection record
--         let conn =
--                 Connection
--                     { peer = peer
--                     , version = version
--                     , started = timestamp
--                     }
--
--         -- Publish connection established event
--         publish $ ConnectionEstablished ConnectionEstablishedData {peer, version, timestamp}
--
--         pure conn
--     Right (Right _) -> do
--         -- This shouldn't happen with InitiatorOnly mode
--         throwError ("Unexpected responder mode result" :: Text)

-- \^ Might be necessary

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
