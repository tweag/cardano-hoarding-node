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

import Cardano.Api.IO (File (..))
import Cardano.Api.LedgerState (mkProtocolInfoCardano, readCardanoGenesisConfig, readNodeConfig)
import Codec.CBOR.Read (DeserialiseFailure)
import Control.Concurrent.Chan.Unagi (InChan, writeChan)
import Control.Exception (AsyncException (..), SomeException, fromException)
import Control.Monad.Trans.Except (runExceptT)
import Control.Tracer (contramap, stdoutTracer)
import Data.Functor.Contravariant ((>$<))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Effectful (Eff, Effect, IOE, Limit (..), Persistence (..), UnliftStrategy (..), liftIO, withEffToIO, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import Network.Mux (Mode (..), StartOnDemandOrEagerly (..))
import Network.Socket (AddrInfo (..), SockAddr, SocketType (Stream))
import Network.TypedProtocol (PeerRole (..))
import Network.TypedProtocol.Peer.Client
import Ouroboros.Consensus.Block.Abstract (headerPoint)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Config (configBlock, configCodec)
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToNodeVersion, supportedNodeToNodeVersions)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Network.Block (castPoint, genesisPoint)
import Ouroboros.Network.Diffusion.Configuration (PeerSharing (..))
import Ouroboros.Network.IOManager (IOManager)
import Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..), MiniProtocolLimits (..), OuroborosApplication (..), OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..), mkMiniProtocolCbFromPeer, mkMiniProtocolCbFromPeerPipelined)
import Ouroboros.Network.NodeToNode
    ( DiffusionMode (..)
    , MiniProtocolParameters (..)
    , NetworkConnectTracers (..)
    , NodeToNodeVersion (..)
    , NodeToNodeVersionData (..)
    , blockFetchMiniProtocolNum
    , chainSyncMiniProtocolNum
    , combineVersions
    , connectTo
    , defaultMiniProtocolParameters
    , keepAliveMiniProtocolNum
    , networkMagic
    , nullNetworkConnectTracers
    , peerSharingMiniProtocolNum
    , simpleSingletonVersions
    , txSubmissionMiniProtocolNum
    )
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress, encodeRemoteAddress)
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Ouroboros.Network.Protocol.KeepAlive.Client (KeepAliveClient (..), KeepAliveClientSt (..), keepAliveClientPeer)
import Ouroboros.Network.Protocol.KeepAlive.Type (Cookie (..))
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient, peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))
import Ouroboros.Network.Snocket (socketSnocket)

import Data.ByteString.Lazy qualified as LBS
import Data.Dynamic qualified as Dyn
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Debug.Trace qualified
import Network.Socket qualified as Socket
import Network.TypedProtocol.Peer.Client qualified as Peer
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.PeerSharing.Client qualified as PeerSharing

import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Log (Log)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Network.Events
    ( ChainSyncEvent (..)
    , ChainSyncIntersectionFoundData (..)
    , ChainSyncStartedData (..)
    , ConnectionEstablishedData (..)
    , ConnectionLostData (..)
    , HandshakeCompletedData (..)
    , Header'
    , HeaderReceivedData (..)
    , NetworkEvent (..)
    , PeerSharingEvent (..)
    , PeerSharingStartedData (..)
    , PeersReceivedData (..)
    , Point'
    , RollBackwardData (..)
    , Tip'
    )
import Hoard.Network.Types (Connection (..))

import Effectful.Exception (catch, throwIO)
import Effectful.Prim (Prim)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Network.Codecs (defaultCodecs)
import Hoard.Effects.Network.Versions (unliftVersions)


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
    :: (Error Text :> es, IOE :> es, Log :> es, Pub :> es, Concurrent :> es, Prim :> es)
    => IOManager
    -> InChan Dyn.Dynamic
    -> FilePath
    -> Eff (Network : es) a
    -> Eff es a
runNetwork ioManager chan protocolConfigPath = interpret $ \_ -> \case
    ConnectToPeer peer -> connectToPeerImpl ioManager chan protocolConfigPath peer
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
    :: forall es
     . (Error Text :> es, IOE :> es, Log :> es, Pub :> es, Prim :> es, Concurrent :> es)
    => IOManager
    -> InChan Dyn.Dynamic
    -> FilePath
    -> Peer
    -> Eff es Connection
connectToPeerImpl ioManager chan protocolConfigPath peer = do
    -- Resolve address
    Log.debug "Resolving peer address..."
    addr <- liftIO $ resolvePeerAddress peer
    Log.debug $ "Resolved to: " <> (T.pack (show addr))

    -- Create connection using ouroboros-network
    Log.debug "Attempting connection..."
    -- Create a publish callback that can be called from IO
    let publishIO :: forall event. (Typeable event) => event -> IO ()
        publishIO event = writeChan chan (Dyn.toDyn event)

    Log.debug "Creating snocket..."
    let snocket = socketSnocket ioManager

    -- Load protocol info and create codecs
    Log.debug "Loading protocol configuration..."
    protocolInfo <- loadProtocolInfo protocolConfigPath
    let codecConfig = configCodec (pInfoConfig protocolInfo)
    let networkMagic = getNetworkMagic (configBlock (pInfoConfig protocolInfo))

    -- Get all supported versions
    let supportedVersions = supportedNodeToNodeVersions (Proxy :: Proxy (CardanoBlock StandardCrypto))

    Log.debug "Creating version-specific codecs and applications..."

    -- Create version data for handshake
    let versionData =
            NodeToNodeVersionData
                { networkMagic
                , diffusionMode = InitiatorOnlyDiffusionMode
                , peerSharing = PeerSharingEnabled -- Enable peer sharing
                , query = False
                }

    -- Helper function to create application for a specific version
    let mkVersionedApp :: NodeToNodeVersion -> BlockNodeToNodeVersion (CardanoBlock StandardCrypto) -> OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString (Eff es) () Void
        mkVersionedApp nodeVersion blockVersion =
            let codecs =
                    defaultCodecs
                        codecConfig
                        blockVersion
                        encodeRemoteAddress
                        (\v -> decodeRemoteAddress v)
                        nodeVersion
            in  mkApplication codecs peer publishIO

    -- Create versions for negotiation - offer all supported versions
    Log.debug "Creating protocol versions..."
    let mkVersions version blockVersion =
            simpleSingletonVersions
                version
                versionData
                (\_ -> mkVersionedApp version blockVersion)

    -- Create versions for all supported protocol versions
    let versions =
            combineVersions
                [ mkVersions nodeVersion blockVersion
                | (nodeVersion, blockVersion) <- Map.toList supportedVersions
                ]

    Log.debug "Codecs created successfully"
    let adhocTracers =
            nullNetworkConnectTracers
                { nctHandshakeTracer = (("[Network] " <>) . show) >$< stdoutTracer
                }

    -- Connect to the peer
    Log.debug "Calling connectTo..."
    let strat = ConcUnlift Persistent Unlimited
    result <-
        withEffToIO strat $ \unlift ->
            connectTo
                snocket
                adhocTracers
                (unliftVersions unlift versions)
                Nothing -- No local address binding
                addr
    Log.debug "connectTo returned!"

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
-- Codec Configuration
--------------------------------------------------------------------------------

-- | Load the Cardano protocol info from config files.
-- This is needed to get the CodecConfig for creating proper codecs.
loadProtocolInfo :: (IOE :> es) => FilePath -> Eff es (ProtocolInfo (CardanoBlock StandardCrypto))
loadProtocolInfo configPath = do
    let configFile = File configPath

    -- Load NodeConfig
    nodeConfigResult <- runExceptT $ readNodeConfig configFile
    nodeConfig <- case nodeConfigResult of
        Left err -> Prelude.error $ "Failed to read node config: " <> T.unpack err
        Right cfg -> pure cfg

    -- Load GenesisConfig
    genesisConfigResult <- runExceptT $ readCardanoGenesisConfig nodeConfig
    genesisConfig <- case genesisConfigResult of
        Left err -> Prelude.error $ "Failed to read genesis config: " <> show err
        Right cfg -> pure cfg

    -- Create ProtocolInfo
    let (protocolInfo, _mkBlockForging) = mkProtocolInfoCardano genesisConfig
    pure protocolInfo


--------------------------------------------------------------------------------
-- Mini-Protocol Application
--------------------------------------------------------------------------------

-- | Wrap a protocol action with exception logging to debug cancellations.
withExceptionLogging :: (IOE :> es) => String -> Eff es a -> Eff es a
withExceptionLogging protocolName action =
    action `catch` \(e :: SomeException) -> do
        case fromException e of
            Just ThreadKilled -> do
                liftIO $ putStrLn $ "[EXCEPTION] " <> protocolName <> " killed: ThreadKilled"
                liftIO $ putStrLn $ "[EXCEPTION] " <> protocolName <> " - This is likely due to the Ki scope cleanup or connection closure"
            Just UserInterrupt -> do
                liftIO $ putStrLn $ "[EXCEPTION] " <> protocolName <> " interrupted: UserInterrupt"
            Just (asyncEx :: AsyncException) -> do
                liftIO $ putStrLn $ "[EXCEPTION] " <> protocolName <> " async exception: " <> show asyncEx
            Nothing -> do
                liftIO $ putStrLn $ "[EXCEPTION] " <> protocolName <> " terminated with exception: " <> show e
        -- Re-throw the exception after logging
        throwIO e


-- | Create the Ouroboros application with all mini-protocols.
--
-- This bundles together ChainSync, BlockFetch, and KeepAlive protocols into
-- an application that runs over the multiplexed connection.
mkApplication
    :: (Concurrent :> es, IOE :> es)
    => Codecs (CardanoBlock StandardCrypto) SockAddr DeserialiseFailure (Eff es) LBS.ByteString LBS.ByteString LBS.ByteString LBS.ByteString LBS.ByteString LBS.ByteString LBS.ByteString
    -> Peer
    -> (forall event. (Typeable event) => event -> IO ())
    -> OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString (Eff es) () Void
mkApplication codecs peer publishEvent =
    OuroborosApplication
        [ -- ChainSync mini-protocol (pipelined)
          MiniProtocol
            { miniProtocolNum = chainSyncMiniProtocolNum
            , miniProtocolLimits = chainSyncLimits
            , miniProtocolStart = StartEagerly
            , miniProtocolRun =
                InitiatorProtocolOnly $
                    mkMiniProtocolCbFromPeerPipelined $
                        \_ ->
                            let codec = cChainSyncCodec codecs
                                -- Note: Exception logging added inside chainSyncClientImpl via Effect
                                client = chainSyncClientImpl peer publishEvent
                                -- Use a tracer to see protocol messages
                                tracer = (("[ChainSync] " <>) . show) >$< stdoutTracer
                            in  (tracer, codec, client)
            }
        , -- BlockFetch mini-protocol (stub - runs forever to avoid terminating)
          MiniProtocol
            { miniProtocolNum = blockFetchMiniProtocolNum
            , miniProtocolLimits = blockFetchLimits
            , miniProtocolStart = StartEagerly
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel ->
                withExceptionLogging "BlockFetch" $ do
                    liftIO $ putStrLn "[DEBUG] BlockFetch protocol stub started - will sleep forever to keep connection alive"
                    -- Sleep forever instead of terminating immediately
                    -- This prevents the stub from signaling connection termination
                    threadDelay maxBound
                    liftIO $ putStrLn "[DEBUG] BlockFetch stub woke up (should never happen)"
                    pure ((), Nothing)
            }
        , -- KeepAlive mini-protocol
          MiniProtocol
            { miniProtocolNum = keepAliveMiniProtocolNum
            , miniProtocolLimits = keepAliveLimits
            , miniProtocolStart = StartEagerly
            , miniProtocolRun =
                InitiatorProtocolOnly $
                    mkMiniProtocolCbFromPeer $
                        \_ ->
                            let codec = cKeepAliveCodec codecs
                                wrappedPeer = Peer.Effect $
                                    withExceptionLogging "KeepAlive" $
                                        do
                                            liftIO $ putStrLn "[DEBUG] KeepAlive protocol started"
                                            pure (keepAliveClientPeer keepAliveClientImpl)
                                tracer = contramap (("[KeepAlive] " <>) . show) stdoutTracer
                            in  (tracer, codec, wrappedPeer)
            }
        , -- PeerSharing mini-protocol
          MiniProtocol
            { miniProtocolNum = peerSharingMiniProtocolNum
            , miniProtocolLimits = peerSharingLimits
            , miniProtocolStart = StartEagerly
            , miniProtocolRun =
                InitiatorProtocolOnly $
                    mkMiniProtocolCbFromPeer $
                        \_ ->
                            let client = peerSharingClientImpl peer publishEvent
                                -- IMPORTANT: Use the version-specific codec from the codecs record!
                                codec = cPeerSharingCodec codecs
                                wrappedPeer = Peer.Effect $ withExceptionLogging "PeerSharing" $ do
                                    timestamp <- liftIO getCurrentTime
                                    liftIO $ publishEvent $ PeerSharingStarted PeerSharingStartedData {peer, timestamp}
                                    liftIO $ putStrLn "[DEBUG] PeerSharing: Published PeerSharingStarted event"
                                    liftIO $ putStrLn "[DEBUG] PeerSharing: About to run peer protocol..."
                                    pure (peerSharingClientPeer client)
                                tracer = contramap (("[PeerSharing] " <>) . show) stdoutTracer
                            in  (tracer, codec, wrappedPeer)
            }
        , -- TxSubmission mini-protocol (stub - runs forever to avoid terminating)
          MiniProtocol
            { miniProtocolNum = txSubmissionMiniProtocolNum
            , miniProtocolLimits = txSubmissionLimits
            , miniProtocolStart = StartEagerly
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel ->
                withExceptionLogging "TxSubmission" $ do
                    liftIO $ putStrLn "[DEBUG] TxSubmission protocol stub started - will sleep forever to keep connection alive"
                    -- Sleep forever instead of terminating immediately
                    -- This prevents the stub from signaling connection termination
                    threadDelay maxBound
                    liftIO $ putStrLn "[DEBUG] TxSubmission stub woke up (should never happen)"
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
-- 3. Waits 1 hour
-- 4. Loops
peerSharingClientImpl
    :: (Concurrent :> es, IOE :> es)
    => Peer
    -> (forall event. (Typeable event) => event -> IO ())
    -> PeerSharingClient SockAddr (Eff es) ()
peerSharingClientImpl peer publishEvent =
    Debug.Trace.trace "[DEBUG] PeerSharing: Creating SendMsgShareRequest..." $
        requestPeers withPeers
  where
    requestPeers = PeerSharing.SendMsgShareRequest $ PeerSharingAmount 100
    withPeers peerAddrs = do
        liftIO $ putStrLn "[DEBUG] PeerSharing: *** CALLBACK EXECUTED - GOT RESPONSE ***"
        liftIO $ putStrLn $ "[DEBUG] PeerSharing: Received response with " <> show (length peerAddrs) <> " peers"
        timestamp <- liftIO getCurrentTime
        let peerAddresses = map (T.pack . show) peerAddrs
            peerCount = length peerAddrs
        liftIO $
            publishEvent $
                PeersReceived
                    PeersReceivedData
                        { peer
                        , peerAddresses
                        , peerCount
                        , timestamp
                        }
        liftIO $ putStrLn "[DEBUG] PeerSharing: Published PeersReceived event"
        liftIO $ putStrLn "[DEBUG] PeerSharing: Waiting 10 seconds"
        threadDelay oneHour
        liftIO $ putStrLn "[DEBUG] PeerSharing: looping"
        pure $ requestPeers withPeers
    oneHour = 3_600_000_000


-- | Create a ChainSync client that synchronizes chain headers (pipelined version).
--
-- This client:
-- 1. Finds an intersection starting from genesis
-- 2. Requests headers continuously
-- 3. Publishes HeaderReceived events for each header
-- 4. Handles rollbacks by publishing RollBackward events
--
-- Note: This runs forever, continuously requesting the next header.
chainSyncClientImpl
    :: forall es
     . (IOE :> es)
    => Peer
    -> (forall event. (Typeable event) => event -> IO ())
    -> PeerPipelined (ChainSync Header' Point' Tip') AsClient ChainSync.StIdle (Eff es) ()
chainSyncClientImpl peer publishEvent =
    ClientPipelined $
        Effect $
            withExceptionLogging "ChainSync" $
                do
                    -- Publish started event
                    timestamp <- liftIO $ getCurrentTime
                    liftIO $ publishEvent $ ChainSyncStarted ChainSyncStartedData {peer, timestamp}
                    liftIO $ putStrLn "[DEBUG] ChainSync: Published ChainSyncStarted event"
                    liftIO $ putStrLn "[DEBUG] ChainSync: Starting pipelined client, finding intersection from genesis"
                    pure findIntersect
  where
    findIntersect :: forall c. Client (ChainSync Header' Point' Tip') (Pipelined Z c) ChainSync.StIdle (Eff es) ()
    findIntersect =
        Yield (ChainSync.MsgFindIntersect [genesisPoint]) $ Await $ \case
            ChainSync.MsgIntersectNotFound {} -> Effect $ do
                liftIO $ putStrLn "[DEBUG] ChainSync: Intersection not found (continuing anyway)"
                pure requestNext
            ChainSync.MsgIntersectFound intersectPt tip -> Effect $ do
                liftIO $ putStrLn "[DEBUG] ChainSync: Intersection found"
                timestamp <- liftIO getCurrentTime
                liftIO $
                    publishEvent $
                        ChainSyncIntersectionFound
                            ChainSyncIntersectionFoundData
                                { peer = peer
                                , point = intersectPt
                                , tip = tip
                                , timestamp = timestamp
                                }
                pure requestNext

    requestNext :: forall c. Client (ChainSync Header' Point' Tip') (Pipelined Z c) ChainSync.StIdle (Eff es) ()
    requestNext =
        Yield ChainSync.MsgRequestNext $ Await $ \case
            ChainSync.MsgRollForward hdr tip -> Effect $ do
                liftIO $ putStrLn "[DEBUG] ChainSync: Received header (RollForward)"
                timestamp <- liftIO getCurrentTime
                let hdrPoint = castPoint $ headerPoint hdr
                liftIO $
                    publishEvent $
                        HeaderReceived
                            HeaderReceivedData
                                { peer = peer
                                , header = hdr
                                , -- TODO point is derived, therefore redundant
                                  point = hdrPoint
                                , tip = tip
                                , timestamp = timestamp
                                }
                pure requestNext
            ChainSync.MsgRollBackward rollbackPt tip -> Effect $ do
                liftIO $ putStrLn "[DEBUG] ChainSync: Rollback"
                timestamp <- liftIO getCurrentTime
                liftIO $
                    publishEvent $
                        RollBackward
                            RollBackwardData
                                { peer = peer
                                , point = rollbackPt
                                , tip = tip
                                , timestamp = timestamp
                                }
                pure requestNext
            ChainSync.MsgAwaitReply -> Await $ \case
                ChainSync.MsgRollForward hdr tip -> Effect $ do
                    liftIO $ putStrLn "[DEBUG] ChainSync: Received header after await (RollForward)"
                    timestamp <- liftIO getCurrentTime
                    let hdrPoint = castPoint $ headerPoint hdr
                    liftIO $
                        publishEvent $
                            HeaderReceived
                                HeaderReceivedData
                                    { peer = peer
                                    , header = hdr
                                    , point = hdrPoint
                                    , tip = tip
                                    , timestamp = timestamp
                                    }
                    pure requestNext
                ChainSync.MsgRollBackward rollbackPt tip -> Effect $ do
                    liftIO $ putStrLn "[DEBUG] ChainSync: Rollback after await"
                    timestamp <- liftIO getCurrentTime
                    liftIO $
                        publishEvent $
                            RollBackward
                                RollBackwardData
                                    { peer = peer
                                    , point = rollbackPt
                                    , tip = tip
                                    , timestamp = timestamp
                                    }
                    pure requestNext


-- | KeepAlive client implementation.
--
-- This client sends periodic keepalive messages to maintain the connection
-- and detect network failures. It sends a message immediately, then waits 10
-- seconds before sending the next one.
keepAliveClientImpl :: (IOE :> es, Concurrent :> es) => KeepAliveClient (Eff es) ()
keepAliveClientImpl = KeepAliveClient sendFirst
  where
    -- Send the first message immediately
    sendFirst = do
        liftIO $ putStrLn "[DEBUG] KeepAlive: Sending first keepalive message"
        pure $ SendMsgKeepAlive (Cookie 42) sendNext

    -- Wait 10 seconds before sending subsequent messages
    sendNext = do
        liftIO $ putStrLn "[DEBUG] KeepAlive: Response received, waiting 10s before next message"
        threadDelay 10_000_000 -- 10 seconds in microseconds
        liftIO $ putStrLn "[DEBUG] KeepAlive: Sending keepalive message"
        pure $ SendMsgKeepAlive (Cookie 42) sendNext
