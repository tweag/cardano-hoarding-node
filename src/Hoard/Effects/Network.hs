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
    , runDiffusion

      -- * Interpreter
    , runNetwork
    ) where

import Control.Concurrent (ThreadId, myThreadId, threadDelay)
import Control.Concurrent.Chan.Unagi (InChan, writeChan)
import Control.Concurrent.Class.MonadSTM.Strict (newTVarIO)
import Control.Exception (IOException, fromException)
import Control.Monad.STM (atomically)
import Control.Tracer (nullTracer)
import Data.Text (Text)
import Data.Time (getCurrentTime, secondsToDiffTime)
import Data.Typeable (Proxy (..), Typeable)
import Data.Void (Void, absurd)
import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import Network.Mux (Mode (..), StartOnDemandOrEagerly (..), nullTracers)
import Network.Mux.Bearer (makeSocketBearer, withReadBufferIO)
import Network.Socket (AddrInfo (..), SockAddr (..), Socket, SocketType (Stream), tupleToHostAddress)
import Ouroboros.Network.ConnectionHandler (MkMuxConnectionHandler (..), classifyHandleError, makeConnectionHandler)
import Ouroboros.Network.ConnectionManager.Types (simplePrunePolicy)
import Ouroboros.Network.Diffusion (Arguments (daInstallSigUSR1Handler), noBindForkPolicy, socketAddressType)
import Ouroboros.Network.Diffusion.Configuration (PeerSelectionTargets (..), PeerSharing (..))
import Ouroboros.Network.Diffusion.Policies (simplePeerSelectionPolicy)
import Ouroboros.Network.Driver.Limits (ProtocolTimeLimits (ProtocolTimeLimits))
import Ouroboros.Network.Driver.Simple (runPeer)
import Ouroboros.Network.ExitPolicy (RepromoteDelay (RepromoteDelay))
import Ouroboros.Network.IOManager (IOManager, IOManagerError)
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolCb (..)
    , MiniProtocolLimits (..)
    , OuroborosApplication (..)
    , OuroborosApplicationWithMinimalCtx
    , OuroborosBundle
    , RunMiniProtocol (..)
    )
import Ouroboros.Network.MuxMode (InResponderMode (NotInResponderMode))
import Ouroboros.Network.NodeToClient (nodeToClientCodecCBORTerm)
import Ouroboros.Network.NodeToNode
    ( AcceptedConnectionsLimit (..)
    , DiffusionMode (..)
    , MiniProtocolParameters (..)
    , NodeToNodeProtocols (..)
    , NodeToNodeVersion (..)
    , NodeToNodeVersionData (..)
    , Versions (..)
    , blockFetchMiniProtocolNum
    , chainSyncMiniProtocolNum
    , combineVersions
    , connectTo
    , defaultMiniProtocolParameters
    , keepAliveMiniProtocolNum
    , nodeToNodeCodecCBORTerm
    , nodeToNodeHandshakeCodec
    , nodeToNodeProtocols
    , nodeToNodeVersionCodec
    , nullNetworkConnectTracers
    , peerSharingMiniProtocolNum
    , simpleSingletonVersions
    , txSubmissionMiniProtocolNum
    )
import Ouroboros.Network.PeerSelection
    ( LedgerPeersConsensusInterface (..)
    , PeerMetricsConfiguration (..)
    , UseLedgerPeers (..)
    , newPeerMetric
    , nullPublicExtraPeersAPI
    , peerChurnGovernor
    )
import Ouroboros.Network.PeerSelection.Governor (PeerSelectionGovernorArgs (..), makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSelection.Governor.Types (ExtraGuardedDecisions (..))
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress, encodeRemoteAddress)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (Config)
import Ouroboros.Network.PeerSharing (bracketPeerSharingClient, newPeerSharingRegistry, requestPeers)
import Ouroboros.Network.Point (origin)
import Ouroboros.Network.Protocol.Handshake
    ( HandshakeArguments (..)
    , acceptableVersion
    , cborTermVersionDataCodec
    , nodeToClientHandshakeCodec
    , queryVersion
    , updateVersionData
    )
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient (..), peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Codec (codecPeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))
import Ouroboros.Network.RethrowPolicy (ErrorCommand (..), ErrorContext (..), RethrowPolicy (..))
import Ouroboros.Network.Snocket (Snocket, socketSnocket)
import Ouroboros.Network.Socket (configureSocket)
import Rel8.Expr.Time (seconds)
import System.Random (mkStdGen)

import Data.ByteString.Lazy qualified as LBS
import Data.Dynamic qualified as Dyn
import Data.Text qualified as T
import Debug.Trace qualified
import Network.Mux qualified as Mx
import Network.Socket qualified as Socket
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.State qualified as CM
import Ouroboros.Network.ConnectionManager.Types qualified as CM
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.Diffusion.Configuration qualified as Diffusion
import Ouroboros.Network.Diffusion.Types qualified as Diffusion

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

runDiffusion :: IOManager -> (forall event. (Typeable event) => event -> IO ()) -> NetworkConfig -> Peer -> IO Void
runDiffusion ioManager publishIO config peer = do
    dcPublicPeerSelectionVar <- makePublicPeerSelectionStateVar
    -- TODO: Make this StdGen less deterministically
    peerSelectionPolicyVar <- newTVarIO $ mkStdGen 2
    peerMetrics <-
        newPeerMetric
            $ PeerMetricsConfiguration
                { -- TODO: The docs say this _must_ equate to the number of
                  -- headers / blocks which are produced per hour. I think this
                  -- is 1 second per block.
                  maxEntriesToTrack = 3600
                }
    daPeerSharingRegistry <- newPeerSharingRegistry
    -- socket <- socket

    let arguments =
            Diffusion.Arguments
                { daNtnDataFlow = const CM.Duplex
                , daNtnPeerSharing = const PeerSharingEnabled
                , daUpdateVersionData = \r diffusionMode -> r {diffusionMode}
                , daNtnHandshakeArguments
                , daNtcHandshakeArguments
                , daLedgerPeersCtx =
                    LedgerPeersConsensusInterface
                        { lpGetLatestSlot = pure origin
                        , lpGetLedgerPeers = pure []
                        , lpExtraAPI = ()
                        }
                , daEmptyExtraState = ()
                , daEmptyExtraCounters = ()
                , daExtraPeersAPI = nullPublicExtraPeersAPI
                , daInstallSigUSR1Handler = \_ _ -> pure ()
                , daPeerSelectionGovernorArgs =
                    PeerSelectionGovernorArgs
                        { abortGovernor = \_ _ -> (Nothing :: Maybe IOException)
                        , updateWithState = \_ _ _ _ -> pure ()
                        , extraDecisions =
                            ExtraGuardedDecisions
                                { preBlocking = \_policy _actions _state -> mempty
                                , postBlocking = \_ _ _ -> mempty
                                , postNonBlocking = \_ _ _ -> mempty
                                , customTargetsAction = Nothing
                                , customLocalRootsAction = Nothing
                                , enableProgressMakingActions = const True
                                , ledgerPeerSnapshotExtraStateChange = Prelude.id
                                }
                        }
                , daPeerSelectionStateToExtraCounters = const ()
                , daToExtraPeers = const ()
                , daRequestPublicRootPeers = Nothing
                , daPeerChurnGovernor = peerChurnGovernor
                , daExtraChurnArgs = ()
                , -- TODO: Might have to set this.
                  daSRVPrefix = ""
                }

        daNtnHandshakeArguments =
            HandshakeArguments
                { haHandshakeTracer = nullTracer
                , haBearerTracer = nullTracer
                , haHandshakeCodec = nodeToNodeHandshakeCodec
                , haVersionDataCodec = cborTermVersionDataCodec nodeToNodeCodecCBORTerm
                , haAcceptVersion = acceptableVersion
                , haQueryVersion = queryVersion
                , haTimeLimits = ProtocolTimeLimits (const $ Just $ secondsToDiffTime 20)
                }

        daNtcHandshakeArguments =
            HandshakeArguments
                { haHandshakeTracer = nullTracer
                , haBearerTracer = nullTracer
                , haHandshakeCodec = nodeToClientHandshakeCodec
                , haVersionDataCodec = cborTermVersionDataCodec nodeToClientCodecCBORTerm
                , haAcceptVersion = acceptableVersion
                , haQueryVersion = queryVersion
                , haTimeLimits = ProtocolTimeLimits (const $ Just $ secondsToDiffTime 20)
                }

        configuration =
            Diffusion.Configuration
                { dcIPv4Address = Just $ Right $ SockAddrInet 1234 (tupleToHostAddress (0, 0, 0, 0))
                , dcIPv6Address = Nothing
                , dcLocalAddress = Nothing
                , dcAcceptedConnectionsLimit =
                    AcceptedConnectionsLimit
                        { acceptedConnectionsHardLimit = 40
                        , acceptedConnectionsSoftLimit = 20
                        , acceptedConnectionsDelay = secondsToDiffTime 5
                        }
                , dcMode = Diffusion.InitiatorOnlyDiffusionMode
                , dcPublicPeerSelectionVar
                , dcPeerSelectionTargets =
                    PeerSelectionTargets
                        { targetNumberOfRootPeers = 10
                        , targetNumberOfKnownPeers = 50
                        , targetNumberOfKnownBigLedgerPeers = 0
                        , targetNumberOfEstablishedPeers = 50
                        , targetNumberOfEstablishedBigLedgerPeers = 0
                        , targetNumberOfActivePeers = 20
                        , targetNumberOfActiveBigLedgerPeers = 0
                        }
                , -- TODO: This could cause us to not find any peers.
                  dcReadLocalRootPeers = pure (mempty :: Config () RelayAccessPoint)
                , dcReadPublicRootPeers = pure mempty
                , dcReadLedgerPeerSnapshot = pure Nothing
                , dcReadUseLedgerPeers = pure DontUseLedgerPeers
                , dcPeerSharing = PeerSharingEnabled
                , dcProtocolIdleTimeout = secondsToDiffTime 10
                , -- Should be the same as the OS configured TIME_WAIT configuration for TCP. This is 60 seconds by default.
                  dcTimeWaitTimeout = secondsToDiffTime 60
                , -- Default value
                  dcDeadlineChurnInterval = secondsToDiffTime 3300
                , -- Default
                  dcBulkChurnInterval = secondsToDiffTime 300
                , dcMuxForkPolicy = noBindForkPolicy
                , dcLocalMuxForkPolicy = noBindForkPolicy
                , dcEgressPollInterval = secondsToDiffTime 10
                }

        versionData =
            NodeToNodeVersionData
                { networkMagic = config.networkMagic
                , diffusionMode = InitiatorOnlyDiffusionMode
                , peerSharing = PeerSharingDisabled
                , query = False
                }

        -- Create versions for negotiation - offer both V_14 and V_15
        -- to increase compatibility with different node versions
        versionsV14 =
            simpleSingletonVersions
                NodeToNodeV_14
                versionData
                $ mkApplication' peer publishIO NodeToNodeV_14
        versionsV15 =
            simpleSingletonVersions
                NodeToNodeV_15
                versionData
                $ mkApplication' peer publishIO NodeToNodeV_15
        versions = combineVersions [versionsV14, versionsV15]

        -- TODO: this policy should also be used in `PeerStateActions` and
        -- `InboundGovernor` (when creating or accepting connections)
        daRethrowPolicy =
            -- Copied from https://github.com/IntersectMBO/ouroboros-network/blob/3690d964143749de226ea354dcc44dc0912bdc31/ouroboros-network/lib/Ouroboros/Network/Diffusion.hs#L272
            -- Only the 'IOManagerError's are fatal, all the other exceptions in the
            -- networking code will only shutdown the bearer (see 'ShutdownPeer' why
            -- this is so).
            RethrowPolicy
                ( \_ctx err ->
                    case fromException err of
                        Just (_ :: IOManagerError) -> ShutdownNode
                        Nothing -> mempty
                )
                <>
                -- IOError rethrow-policy
                --
                -- After a critical bug, we decided that `IOError` policy should only
                -- kill the connection which thrown it.  `IOError`s are not propagated.
                -- There's a risk that one could arm an attack if one discovers
                -- a mechanism to trigger fatal `IOError`s, e.g. through a kernel bug.
                --
                -- It is responsibility for an SPO to monitor the node if it is making
                -- progress and have enough resources to do so, e.g. if it has enough
                -- memory, file descriptors.
                --
                -- The `ouroboros-network` guarantees running on a fixed number of file
                -- descriptors given a topology file, see
                -- https://github.com/IntersectMBO/ouroboros-network/issues/4585#issuecomment-1591777447
                -- There's also a calculation for `ouroboros-consensus`, see
                -- https://github.com/IntersectMBO/ouroboros-consensus/issues/20#issuecomment-1514554680
                -- File descriptors could be drained by the tracing system in
                -- `cardano-node` (such a bug existed), or even an external process.
                --
                RethrowPolicy
                    ( \_ctx err ->
                        case fromException err :: Maybe IOException of
                            Just {} -> mempty
                            Nothing -> mempty
                    )
                <> RethrowPolicy
                    ( \ctx err -> case (ctx, fromException err) of
                        (OutboundError, Just Mx.UnknownMiniProtocol {}) ->
                            ShutdownPeer
                        _ -> mempty
                    )
        applications =
            Diffusion.Applications
                { daApplicationInitiatorMode = versions
                , daApplicationInitiatorResponderMode = Versions mempty
                , daLocalResponderApplication = Versions mempty
                , daRethrowPolicy
                , daReturnPolicy = \_ -> RepromoteDelay $ secondsToDiffTime 20
                , daRepromoteErrorDelay = RepromoteDelay $ secondsToDiffTime 30
                , daLocalRethrowPolicy = daRethrowPolicy
                , daPeerSelectionPolicy = simplePeerSelectionPolicy peerSelectionPolicyVar peerMetrics
                , daPeerSharingRegistry
                }
    interfaces <- Diffusion.mkInterfaces ioManager nullTracer 0.2
    Diffusion.runM interfaces Diffusion.nullTracers arguments configuration applications


withConnectionManager
    :: ThreadId
    -> NetworkMagic
    -> (forall e. (Typeable e) => e -> IO ())
    -> Peer
    -> Snocket IO Socket SockAddr
    -> (forall handle handleError. (Show handleError) => CM.ConnectionManager InitiatorMode Socket SockAddr handle handleError IO -> IO a)
    -> IO a
withConnectionManager mainThreadId networkMagic publishIO peer snocket action = do
    connStateIdSupply <- atomically $ CM.newConnStateIdSupply Proxy
    let args =
            CM.Arguments
                { CM.tracer = nullTracer
                , CM.trTracer = nullTracer
                , -- TODO: Might be necessary
                  CM.ipv4Address = Nothing
                , CM.ipv6Address = Nothing
                , CM.addressType = socketAddressType
                , CM.snocket = snocket
                , CM.makeBearer = makeSocketBearer
                , CM.withBuffer = withReadBufferIO
                , CM.configureSocket = configureSocket
                , CM.timeWaitTimeout = secondsToDiffTime 60
                , CM.outboundIdleTimeout = secondsToDiffTime 10
                , -- TODO: This might cause issues is the connection is not
                  -- Duplex, which it might not be since we're not setting
                  -- `ipcv4Address` or `ipv6Address`.
                  CM.connectionDataFlow = const CM.Duplex
                , -- TODO: Evaluate whether this is the right prune policy. This is
                  -- what's used in `Ouroboros.Network.Diffusion` for
                  -- `InitiatorOnly` mode.
                  CM.prunePolicy = simplePrunePolicy
                , -- TODO: should probably not be made deterministically like
                  -- this.
                  CM.stdGen = mkStdGen 1
                , -- TODO: Need to refine these numbers. I've literally just
                  -- pulled them out of thin air.
                  CM.connectionsLimits =
                    AcceptedConnectionsLimit
                        { acceptedConnectionsHardLimit = 40
                        , acceptedConnectionsSoftLimit = 20
                        , acceptedConnectionsDelay = secondsToDiffTime 5
                        }
                , CM.connStateIdSupply
                , CM.classifyHandleError
                , CM.updateVersionData = \vd diffusionMode -> vd {diffusionMode}
                }
        handshakeArguments =
            HandshakeArguments
                { haHandshakeTracer = nullTracer
                , haBearerTracer = nullTracer
                , haHandshakeCodec = nodeToNodeHandshakeCodec
                , haVersionDataCodec = cborTermVersionDataCodec nodeToNodeCodecCBORTerm
                , haAcceptVersion = acceptableVersion
                , haQueryVersion = queryVersion
                , haTimeLimits = ProtocolTimeLimits (const $ Just $ secondsToDiffTime 20)
                }
        connectionHandler =
            makeConnectionHandler
                nullTracers
                noBindForkPolicy
                handshakeArguments
                versions
                (mainThreadId, rethrowPolicy)
                MuxInitiatorConnectionHandler

    CM.with args NotInResponderMode connectionHandler action
  where
    -- TODO: this policy should also be used in `PeerStateActions` and
    -- `InboundGovernor` (when creating or accepting connections)
    rethrowPolicy =
        -- Copied from https://github.com/IntersectMBO/ouroboros-network/blob/3690d964143749de226ea354dcc44dc0912bdc31/ouroboros-network/lib/Ouroboros/Network/Diffusion.hs#L272
        -- Only the 'IOManagerError's are fatal, all the other exceptions in the
        -- networking code will only shutdown the bearer (see 'ShutdownPeer' why
        -- this is so).
        RethrowPolicy
            ( \_ctx err ->
                case fromException err of
                    Just (_ :: IOManagerError) -> ShutdownNode
                    Nothing -> mempty
            )
            <>
            -- IOError rethrow-policy
            --
            -- After a critical bug, we decided that `IOError` policy should only
            -- kill the connection which thrown it.  `IOError`s are not propagated.
            -- There's a risk that one could arm an attack if one discovers
            -- a mechanism to trigger fatal `IOError`s, e.g. through a kernel bug.
            --
            -- It is responsibility for an SPO to monitor the node if it is making
            -- progress and have enough resources to do so, e.g. if it has enough
            -- memory, file descriptors.
            --
            -- The `ouroboros-network` guarantees running on a fixed number of file
            -- descriptors given a topology file, see
            -- https://github.com/IntersectMBO/ouroboros-network/issues/4585#issuecomment-1591777447
            -- There's also a calculation for `ouroboros-consensus`, see
            -- https://github.com/IntersectMBO/ouroboros-consensus/issues/20#issuecomment-1514554680
            -- File descriptors could be drained by the tracing system in
            -- `cardano-node` (such a bug existed), or even an external process.
            --
            RethrowPolicy
                ( \_ctx err ->
                    case fromException err :: Maybe IOException of
                        Just {} -> mempty
                        Nothing -> mempty
                )
            <> RethrowPolicy
                ( \ctx err -> case (ctx, fromException err) of
                    (OutboundError, Just Mx.UnknownMiniProtocol {}) ->
                        ShutdownPeer
                    _ -> mempty
                )
    versionData =
        NodeToNodeVersionData
            { networkMagic = networkMagic
            , diffusionMode = InitiatorOnlyDiffusionMode
            , peerSharing = PeerSharingDisabled
            , query = False
            }

    -- Create versions for negotiation - offer both V_14 and V_15
    -- to increase compatibility with different node versions
    versionsV14 =
        simpleSingletonVersions
            NodeToNodeV_14
            versionData
            $ mkApplication' peer publishIO NodeToNodeV_14
    versionsV15 =
        simpleSingletonVersions
            NodeToNodeV_15
            versionData
            $ mkApplication' peer publishIO NodeToNodeV_15
    versions = combineVersions [versionsV14, versionsV15]


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
    :: Peer
    -> (forall event. (Typeable event) => event -> IO ())
    -- ^ Publish event callback
    -> OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void
mkApplication peer publishEvent =
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
        , -- PeerSharing mini-protocol (stub)
          MiniProtocol
            { miniProtocolNum = peerSharingMiniProtocolNum
            , miniProtocolLimits = peerSharingLimits
            , miniProtocolStart = StartOnDemand
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] PeerSharing protocol stub started"
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


mkApplication'
    :: Peer
    -> (forall event. (Typeable event) => event -> IO ())
    -> NodeToNodeVersion
    -> NodeToNodeVersionData
    -> OuroborosBundle InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
mkApplication' peer publishEvent =
    nodeToNodeProtocols
        defaultMiniProtocolParameters
        ( NodeToNodeProtocols
            { chainSyncProtocol =
                InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                    -- Stub: For Ticket #1, just return immediately to allow connection to complete
                    -- Real implementation in Ticket #3 will do actual protocol handshake
                    putStrLn "[DEBUG] ChainSync protocol stub started"
                    pure ((), Nothing)
            , blockFetchProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] BlockFetch protocol stub started"
                pure ((), Nothing)
            , txSubmissionProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] TxSubmission protocol stub started"
                pure ((), Nothing)
            , keepAliveProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel -> do
                -- Stub: For Ticket #1, just return immediately
                putStrLn "[DEBUG] KeepAlive protocol stub started"
                pure ((), Nothing)
            , peerSharingProtocol = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx channel -> do
                -- Publish started event
                timestamp <- getCurrentTime
                publishEvent $ PeerSharingStarted PeerSharingStartedData {peer, timestamp}
                putStrLn "[DEBUG] PeerSharing: Published PeerSharingStarted event"

                -- Create and run the PeerSharing client
                let client = peerSharingClientImpl peer publishEvent
                let clientPeer = peerSharingClientPeer client
                    codec = codecPeerSharing (encodeRemoteAddress undefined) (decodeRemoteAddress undefined)

                putStrLn "[DEBUG] PeerSharing: About to run peer protocol..."
                -- Run the protocol
                _ <- runPeer nullTracer codec channel clientPeer
                putStrLn "[DEBUG] PeerSharing: Protocol completed"
                pure ((), Nothing)
            }
        )


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
    Debug.Trace.trace "[DEBUG] PeerSharing: Creating SendMsgShareRequest..."
        $ SendMsgShareRequest (PeerSharingAmount 100)
        $ \peerAddrs -> do
            putStrLn "[DEBUG] PeerSharing: *** CALLBACK EXECUTED - GOT RESPONSE ***"
            putStrLn $ "[DEBUG] PeerSharing: Received response with " <> show (length peerAddrs) <> " peers"
            timestamp <- getCurrentTime
            let peerAddrTexts = map (T.pack . show) peerAddrs
                peerCount = length peerAddrs
            publishEvent
                $ PeersReceived
                    PeersReceivedData
                        { peer = peer
                        , peerAddresses = peerAddrTexts
                        , peerCount = peerCount
                        , timestamp = timestamp
                        }
            putStrLn "[DEBUG] PeerSharing: Published PeersReceived event"
            pure $ SendMsgDone (pure ())
