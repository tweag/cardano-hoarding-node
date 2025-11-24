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
import Control.Concurrent.Chan.Unagi (newChan)
import Control.Tracer (Tracer (..), stdoutTracer)
import Effectful (Eff, Effect, IOE, Limit (..), Persistence (..), UnliftStrategy (..), withEffToIO, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import Network.Mux (Mode (..), StartOnDemandOrEagerly (..))
import Network.Socket (SockAddr)
import Network.TypedProtocol (PeerRole (..))
import Network.TypedProtocol.Peer.Client
import Ouroboros.Consensus.Block.Abstract (headerPoint)
import Ouroboros.Consensus.Config (configBlock, configCodec)
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..), defaultCodecs)
import Ouroboros.Consensus.Node.NetworkProtocolVersion (supportedNodeToNodeVersions)
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
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Ouroboros.Network.Protocol.KeepAlive.Client (KeepAliveClient (..), KeepAliveClientSt (..), keepAliveClientPeer)
import Ouroboros.Network.Protocol.KeepAlive.Type (Cookie (..))
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient, peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))
import Ouroboros.Network.Snocket (socketSnocket)

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Network.TypedProtocol.Peer.Client qualified as Peer
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.PeerSharing.Client qualified as PeerSharing

import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (PeerAddress (..), sockAddrToPeerAddress)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Log (Log)
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Network.Events
    ( BlockBatchCompletedData (..)
    , BlockFetchEvent (..)
    , BlockFetchFailedData (..)
    , BlockFetchRequest (..)
    , BlockFetchStartedData (..)
    , BlockReceivedData (..)
    , ChainSyncEvent (..)
    , ChainSyncIntersectionFoundData (..)
    , ChainSyncStartedData (..)
    , ConnectionEstablishedData (..)
    , ConnectionLostData (..)
    , HandshakeCompletedData (..)
    , HeaderReceivedData (..)
    , NetworkEvent (..)
    , PeerSharingEvent (..)
    , PeerSharingStartedData (..)
    , PeersReceivedData (..)
    , RollBackwardData (..)
    )
import Hoard.Network.Types (Connection (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader, CardanoPoint, CardanoTip)

import Data.IP qualified as IP
import Data.Set qualified as S
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Input (Input, input, runInputChan)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Network.Codecs (hoistCodecs)
import Hoard.Effects.Output (Output, output, runOutputChan)
import Hoard.Effects.Sub (Sub)
import Hoard.Effects.Sub qualified as Sub
import Hoard.Types.NodeIP (NodeIP (..))
import Ouroboros.Network.Protocol.BlockFetch.Client (blockFetchClientPeer)


--------------------------------------------------------------------------------
-- Network Effect
--------------------------------------------------------------------------------

-- | Effect for managing peer connections.
--
-- Provides operations to connect to peers, disconnect, and check connection status.
data Network :: Effect where
    ConnectToPeer :: PeerAddress -> Network m Connection
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
    :: ( Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Error Text :> es
       , IOE :> es
       , Log :> es
       , Pub :> es
       , Sub :> es
       )
    => IOManager
    -> FilePath
    -> Eff (Network : es) a
    -> Eff es a
runNetwork ioManager protocolConfigPath = interpret $ \_ -> \case
    ConnectToPeer peer -> connectToPeerImpl ioManager protocolConfigPath peer
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
     . ( Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Error Text :> es
       , IOE :> es
       , Log :> es
       , Pub :> es
       , Sub :> es
       )
    => IOManager
    -> FilePath
    -> PeerAddress
    -> Eff es Connection
connectToPeerImpl ioManager protocolConfigPath peer = do
    let addr = IP.toSockAddr (getNodeIP peer.host, fromIntegral peer.port)
    -- Create connection using ouroboros-network
    Log.debug $ "Attempting connection to " <> show addr

    Log.debug "Creating snocket..."
    let snocket = socketSnocket ioManager

    -- Load protocol info and create codecs
    Log.debug "Loading protocol configuration..."
    protocolInfo <- loadProtocolInfo protocolConfigPath
    let codecConfig = configCodec (pInfoConfig protocolInfo)
    let networkMagic = getNetworkMagic (configBlock (pInfoConfig protocolInfo))

    -- Get all supported versions
    let supportedVersions = supportedNodeToNodeVersions (Proxy :: Proxy CardanoBlock)

    Log.debug "Creating version-specific codecs and applications..."

    -- Create version data for handshake
    let versionData =
            NodeToNodeVersionData
                { networkMagic
                , diffusionMode = InitiatorOnlyDiffusionMode
                , peerSharing = PeerSharingEnabled -- Enable peer sharing
                , query = False
                }

    (blockFetchInChan, blockFetchOutChan) <- liftIO newChan

    -- Helper function to create application for a specific version
    let strat = ConcUnlift Persistent Unlimited
        mkVersionedApp (unlift :: forall x. Eff (Input BlockFetchRequest : Output BlockFetchRequest : es) x -> IO x) nodeVersion blockVersion =
            let codecs =
                    hoistCodecs liftIO $
                        defaultCodecs
                            codecConfig
                            blockVersion
                            encodeRemoteAddress
                            (\v -> decodeRemoteAddress v)
                            nodeVersion
            in  mkApplication unlift codecs peer

    -- Create versions for negotiation - offer all supported versions
    Log.debug "Creating protocol versions..."
    let mkVersions (unlift :: forall x. Eff (Input BlockFetchRequest : Output BlockFetchRequest : es) x -> IO x) version blockVersion =
            simpleSingletonVersions
                version
                versionData
                (\_ -> mkVersionedApp unlift version blockVersion)

    -- Create versions for all supported protocol versions
    let versions (unlift :: forall x. Eff (Input BlockFetchRequest : Output BlockFetchRequest : es) x -> IO x) =
            combineVersions
                [ mkVersions unlift nodeVersion blockVersion
                | (nodeVersion, blockVersion) <- Map.toList supportedVersions
                ]

    Log.debug "Codecs created successfully"
    adhocTracers <- withEffToIO strat $ \unlift ->
        pure $
            nullNetworkConnectTracers
                { nctHandshakeTracer = (("[Network] " <>) . show) >$< logTracer unlift Log.DEBUG
                }

    -- Connect to the peer
    Log.debug "Calling connectTo..."
    result <- runOutputChan blockFetchInChan . runInputChan blockFetchOutChan $ do
        Conc.fork_ pickBlockFetchRequest
        withEffToIO strat $ \unlift ->
            connectTo
                snocket
                adhocTracers
                (versions unlift)
                Nothing -- No local address binding
                addr
    Log.debug "connectTo returned!"

    case result of
        Left err -> do
            throwError @Text $ "Failed to connect to peer " <> show peer <> ": " <> show err
        Right (Left ()) -> do
            -- Connection succeeded, create Connection record
            timestamp <- Clock.currentTime
            let version = NodeToNodeV_14 -- We negotiated this version

            -- Publish handshake completed event
            publish $ HandshakeCompleted HandshakeCompletedData {peer, version, timestamp}

            -- Note: The mini-protocols are already running in the background
            -- via the application we passed to connectTo

            -- Create connection record
            let conn =
                    Connection
                        { peer
                        , version
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
    :: (Pub :> es, Clock :> es)
    => Connection
    -> Eff es ()
disconnectPeerImpl conn = do
    -- Publish connection lost event
    timestamp <- Clock.currentTime
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
-- Codec Configuration
--------------------------------------------------------------------------------

-- | Load the Cardano protocol info from config files.
-- This is needed to get the CodecConfig for creating proper codecs.
loadProtocolInfo :: (IOE :> es) => FilePath -> Eff es (ProtocolInfo CardanoBlock)
loadProtocolInfo configPath = do
    let configFile = File configPath

    -- Load NodeConfig
    nodeConfigResult <- runExceptT $ readNodeConfig configFile
    nodeConfig <- case nodeConfigResult of
        Left err -> error $ "Failed to read node config: " <> err
        Right cfg -> pure cfg

    -- Load GenesisConfig
    genesisConfigResult <- runExceptT $ readCardanoGenesisConfig nodeConfig
    genesisConfig <- case genesisConfigResult of
        Left err -> error $ "Failed to read genesis config: " <> show err
        Right cfg -> pure cfg

    -- Create ProtocolInfo
    let (protocolInfo, _mkBlockForging) = mkProtocolInfoCardano genesisConfig
    pure protocolInfo


--------------------------------------------------------------------------------
-- Mini-Protocol Application
--------------------------------------------------------------------------------

-- | Create the Ouroboros application with all mini-protocols.
--
-- This bundles together ChainSync, BlockFetch, and KeepAlive protocols into
-- an application that runs over the multiplexed connection.
mkApplication
    :: ( Clock :> es
       , Concurrent :> es
       , Input BlockFetchRequest :> es
       , Log :> es
       , Pub :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Codecs
        CardanoBlock
        SockAddr
        DeserialiseFailure
        IO
        LBS.ByteString
        LBS.ByteString
        LBS.ByteString
        LBS.ByteString
        LBS.ByteString
        LBS.ByteString
        LBS.ByteString
    -> PeerAddress
    -> OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void
mkApplication unlift codecs peer =
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
                                client = chainSyncClientImpl unlift peer
                                -- Use a tracer to see protocol messages
                                tracer = (("[ChainSync] " <>) . show) >$< logTracer unlift Log.DEBUG
                            in  (tracer, codec, client)
            }
        , -- BlockFetch mini-protocol
          MiniProtocol
            { miniProtocolNum = blockFetchMiniProtocolNum
            , miniProtocolLimits = blockFetchLimits
            , miniProtocolStart = StartEagerly
            , miniProtocolRun = InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer $ \_ ->
                let codec = cBlockFetchCodec codecs
                    client = blockFetchClientImpl unlift peer
                    tracer = (("[BlockFetch] " <>) . show) >$< stdoutTracer
                    wrappedPeer = Peer.Effect $ unlift $ withExceptionLogging "BlockFetch" $ do
                        Log.debug "BlockFetch protocol started"
                        pure $ blockFetchClientPeer client
                in  (tracer, codec, wrappedPeer)
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
                                    unlift $
                                        withExceptionLogging "KeepAlive" $ do
                                            Log.debug "KeepAlive protocol started"
                                            pure (keepAliveClientPeer $ keepAliveClientImpl unlift)
                                tracer = contramap (("[KeepAlive] " <>) . show) $ logTracer unlift Log.DEBUG
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
                            let client = peerSharingClientImpl unlift peer
                                -- IMPORTANT: Use the version-specific codec from the codecs record!
                                codec = cPeerSharingCodec codecs
                                wrappedPeer = Peer.Effect $ unlift $ withExceptionLogging "PeerSharing" $ do
                                    timestamp <- Clock.currentTime
                                    publish $ PeerSharingStarted PeerSharingStartedData {peer, timestamp}
                                    Log.debug "PeerSharing: Published PeerSharingStarted event"
                                    Log.debug "PeerSharing: About to run peer protocol..."
                                    pure (peerSharingClientPeer client)
                                tracer = contramap (("[PeerSharing] " <>) . show) $ logTracer unlift Log.DEBUG
                            in  (tracer, codec, wrappedPeer)
            }
        , -- TxSubmission mini-protocol (stub - runs forever to avoid terminating)
          MiniProtocol
            { miniProtocolNum = txSubmissionMiniProtocolNum
            , miniProtocolLimits = txSubmissionLimits
            , miniProtocolStart = StartEagerly
            , miniProtocolRun = InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx _channel ->
                unlift $ withExceptionLogging "TxSubmission" $ do
                    Log.debug "TxSubmission protocol stub started - will sleep forever to keep connection alive"
                    -- Sleep forever instead of terminating immediately
                    -- This prevents the stub from signaling connection termination
                    threadDelay maxBound
                    Log.debug "TxSubmission stub woke up (should never happen)"
                    pure ((), Nothing)
            }
        ]
  where
    params = defaultMiniProtocolParameters

    chainSyncLimits =
        MiniProtocolLimits
            { maximumIngressQueue = fromIntegral $ chainSyncPipeliningHighMark params * 4
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
    :: (Concurrent :> es, Clock :> es, Log :> es, Pub :> es)
    => (forall x. Eff es x -> IO x)
    -> PeerAddress
    -> PeerSharingClient SockAddr IO ()
peerSharingClientImpl unlift peer = requestPeers withPeers
  where
    requestPeers = PeerSharing.SendMsgShareRequest $ PeerSharingAmount 100
    withPeers peerAddrs = unlift do
        Log.debug "PeerSharing: *** CALLBACK EXECUTED - GOT RESPONSE ***"
        Log.debug $ "PeerSharing: Received response with " <> show (length peerAddrs) <> " peers"
        timestamp <- Clock.currentTime
        publish $
            PeersReceived
                PeersReceivedData
                    { peer
                    , peerAddresses = S.fromList $ mapMaybe sockAddrToPeerAddress peerAddrs
                    , timestamp
                    }
        Log.debug "PeerSharing: Published PeersReceived event"
        Log.debug "PeerSharing: Waiting 10 seconds"
        threadDelay oneHour
        Log.debug "PeerSharing: looping"
        pure $ requestPeers withPeers
    oneHour = 3_600_000_000


-- | Create a BlockFetch client that fetches blocks on request over a channel.
--
-- This client:
blockFetchClientImpl
    :: forall es
     . ( Pub :> es
       , Log :> es
       , Input BlockFetchRequest :> es
       , Clock :> es
       , Concurrent :> es
       )
    => (forall x. Eff es x -> IO x)
    -> PeerAddress
    -> BlockFetch.BlockFetchClient CardanoBlock CardanoPoint IO ()
blockFetchClientImpl unlift peer =
    BlockFetch.BlockFetchClient $ unlift $ do
        timestamp <- Clock.currentTime
        publish $ BlockFetchStarted $ BlockFetchStartedData {timestamp, peer}
        Log.debug "BlockFetch: Published BlockFetchStarted event"
        Log.debug "BlockFetch: Starting client, awaiting block download requests"
        awaitMessage
  where
    awaitMessage :: Eff es (BlockFetch.BlockFetchRequest CardanoBlock CardanoPoint IO ())
    awaitMessage = do
        req <- input
        pure
            $ BlockFetch.SendMsgRequestRange
                (BlockFetch.ChainRange req.point req.point)
                (handleResponse req)
            $ blockFetchClientImpl unlift peer

    handleResponse req =
        BlockFetch.BlockFetchResponse
            { handleStartBatch =
                pure $ blockReceiver 0
            , handleNoBlocks = unlift $ do
                timestamp <- Clock.currentTime
                publish $
                    BlockFetchFailed $
                        BlockFetchFailedData
                            { peer
                            , point = req.point
                            , errorMessage = "No blocks for point"
                            , timestamp
                            }
            }

    blockReceiver blockCount =
        BlockFetch.BlockFetchReceiver
            { handleBlock = \block -> unlift $ do
                timestamp <- Clock.currentTime
                publish $
                    BlockReceived $
                        BlockReceivedData
                            { peer
                            , block
                            , timestamp
                            }
                pure $ blockReceiver $ blockCount + 1
            , handleBatchDone = unlift $ do
                timestamp <- Clock.currentTime
                publish $
                    BlockBatchCompleted $
                        BlockBatchCompletedData
                            { peer
                            , blockCount
                            , timestamp
                            }
            }


-- | Re-emit `HeaderReceived` events as `BlockFetchRequests`.
pickBlockFetchRequest
    :: ( Clock :> es
       , Output BlockFetchRequest :> es
       , Sub :> es
       )
    => Eff es Void
pickBlockFetchRequest = Sub.listen $ \case
    HeaderReceived dat -> do
        timestamp <- Clock.currentTime
        output $
            BlockFetchRequest
                { timestamp
                , point = dat.point
                , peer = dat.peer
                }
    _ -> pure ()


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
     . ( Clock :> es
       , Log :> es
       , Pub :> es
       )
    => (forall x. Eff es x -> IO x)
    -> PeerAddress
    -> PeerPipelined (ChainSync CardanoHeader CardanoPoint CardanoTip) AsClient ChainSync.StIdle IO ()
chainSyncClientImpl unlift peer =
    ClientPipelined $
        Effect $
            unlift $
                withExceptionLogging "ChainSync" $
                    do
                        -- Publish started event
                        timestamp <- Clock.currentTime
                        publish $ ChainSyncStarted ChainSyncStartedData {peer, timestamp}
                        Log.debug "ChainSync: Published ChainSyncStarted event"
                        Log.debug "ChainSync: Starting pipelined client, finding intersection from genesis"
                        pure findIntersect
  where
    findIntersect :: forall c. Client (ChainSync CardanoHeader CardanoPoint CardanoTip) (Pipelined Z c) ChainSync.StIdle IO ()
    findIntersect =
        Yield (ChainSync.MsgFindIntersect [genesisPoint]) $ Await $ \case
            ChainSync.MsgIntersectNotFound {} -> Effect $ unlift $ do
                Log.debug "ChainSync: Intersection not found (continuing anyway)"
                pure requestNext
            ChainSync.MsgIntersectFound point tip -> Effect $ unlift $ do
                Log.debug "ChainSync: Intersection found"
                timestamp <- Clock.currentTime
                publish $
                    ChainSyncIntersectionFound
                        ChainSyncIntersectionFoundData
                            { peer
                            , point
                            , tip
                            , timestamp
                            }
                pure requestNext

    requestNext :: forall c. Client (ChainSync CardanoHeader CardanoPoint CardanoTip) (Pipelined Z c) ChainSync.StIdle IO ()
    requestNext =
        Yield ChainSync.MsgRequestNext $ Await $ \case
            ChainSync.MsgRollForward header tip -> Effect $ unlift $ do
                Log.debug "ChainSync: Received header (RollForward)"
                timestamp <- Clock.currentTime
                let point = castPoint $ headerPoint header
                publish $
                    HeaderReceived
                        HeaderReceivedData
                            { peer
                            , header
                            , -- TODO point is derived, therefore redundant
                              point
                            , tip
                            , timestamp
                            }
                pure requestNext
            ChainSync.MsgRollBackward point tip -> Effect $ unlift $ do
                Log.debug "ChainSync: Rollback"
                timestamp <- Clock.currentTime
                publish $
                    RollBackward
                        RollBackwardData
                            { peer
                            , point
                            , tip
                            , timestamp
                            }
                pure requestNext
            ChainSync.MsgAwaitReply -> Await $ \case
                ChainSync.MsgRollForward header tip -> Effect $ unlift $ do
                    Log.debug "ChainSync: Received header after await (RollForward)"
                    timestamp <- Clock.currentTime
                    let point = castPoint $ headerPoint header
                    publish $
                        HeaderReceived
                            HeaderReceivedData
                                { peer
                                , header
                                , point
                                , tip
                                , timestamp
                                }
                    pure requestNext
                ChainSync.MsgRollBackward point tip -> Effect $ unlift $ do
                    Log.debug "ChainSync: Rollback after await"
                    timestamp <- Clock.currentTime
                    publish $
                        RollBackward
                            RollBackwardData
                                { peer
                                , point
                                , tip
                                , timestamp
                                }
                    pure requestNext


-- | KeepAlive client implementation.
--
-- This client sends periodic keepalive messages to maintain the connection
-- and detect network failures. It sends a message immediately, then waits 10
-- seconds before sending the next one.
keepAliveClientImpl :: (Concurrent :> es, Log :> es) => (forall x. Eff es x -> IO x) -> KeepAliveClient IO ()
keepAliveClientImpl unlift = KeepAliveClient sendFirst
  where
    -- Send the first message immediately
    sendFirst = unlift $ do
        Log.debug "KeepAlive: Sending first keepalive message"
        pure $ SendMsgKeepAlive (Cookie 42) sendNext

    -- Wait 10 seconds before sending subsequent messages
    sendNext = unlift $ do
        Log.debug "KeepAlive: Response received, waiting 10s before next message"
        threadDelay 10_000_000 -- 10 seconds in microseconds
        Log.debug "KeepAlive: Sending keepalive message"
        pure $ SendMsgKeepAlive (Cookie 42) sendNext


logTracer :: (Log :> es) => (forall x. Eff es x -> m x) -> Log.Severity -> Tracer m String
logTracer unlift severity =
    Tracer $ \msg -> unlift $ Log.log severity $ toText msg
