-- |
-- Module: Hoard.Effects.NodeToNode
-- Description: Effect for managing peer connections
--
-- This effect provides high-level operations for connecting to Cardano peers
-- and managing the node-to-node protocol communication.
module Hoard.Effects.NodeToNode
    ( -- * Effect
      NodeToNode
    , Config (..)
    , connectToPeer

      -- * Interpreter
    , runNodeToNode
    ) where

import Cardano.Api ()
import Data.ByteString.Lazy qualified as LBS
import Data.IP qualified as IP
import Data.List (maximum, minimum)
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Effectful (Eff, Effect, IOE, Limit (..), Persistence (..), UnliftStrategy (..), withEffToIO, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Concurrent.QSem (signalQSem, waitQSem)
import Effectful.Dispatch.Dynamic (interpret, localUnlift)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask, asks)
import Effectful.State.Static.Shared (State)
import Effectful.TH (makeEffect)
import Network.Mux (Mode (..), StartOnDemandOrEagerly (..))
import Network.Socket (SockAddr)
import Network.TypedProtocol.Peer.Client qualified as Peer
import Ouroboros.Consensus.Block (headerPoint)
import Ouroboros.Consensus.Config (configBlock, configCodec)
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..), defaultCodecs)
import Ouroboros.Consensus.Node.NetworkProtocolVersion (supportedNodeToNodeVersions)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Network.Diffusion.Configuration (PeerSharing (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolCb (..)
    , OuroborosApplication (..)
    , OuroborosApplicationWithMinimalCtx
    , RunMiniProtocol (..)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.NodeToNode
    ( DiffusionMode (..)
    , NetworkConnectTracers (..)
    , NodeToNodeVersionData (..)
    , blockFetchMiniProtocolNum
    , combineVersions
    , connectTo
    , keepAliveMiniProtocolNum
    , networkMagic
    , nullNetworkConnectTracers
    , peerSharingMiniProtocolNum
    , simpleSingletonVersions
    , txSubmissionMiniProtocolNum
    )
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress, encodeRemoteAddress)
import Ouroboros.Network.Protocol.BlockFetch.Client (blockFetchClientPeer)
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch
import Ouroboros.Network.Protocol.KeepAlive.Client (KeepAliveClient (..), KeepAliveClientSt (..), keepAliveClientPeer)
import Ouroboros.Network.Protocol.KeepAlive.Type (Cookie (..))
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient, peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Client qualified as PeerSharing
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))
import Ouroboros.Network.Snocket (socketSnocket)
import Prelude hiding (Reader, State, ask, asks, evalState, get, gets)

import Hoard.ChainSync.NodeToNode qualified as ChainSync
import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..), PeerAddress (..), sockAddrToPeerAddress)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc (concStrat)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode.Codecs (hoistCodecs)
import Hoard.Effects.NodeToNode.Config (Config (..))
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Network.Events
    ( BlockBatchCompleted (..)
    , BlockFetchFailed (..)
    , BlockFetchRequest (..)
    , BlockFetchStarted (..)
    , BlockReceived (..)
    , PeerSharingStarted (..)
    , PeersReceived (..)
    )
import Hoard.Types.Cardano (CardanoBlock, CardanoCodecs, CardanoPoint)
import Hoard.Types.Environment (Env)
import Hoard.Types.Environment qualified as Env
import Hoard.Types.HoardState (HoardState (..))
import Hoard.Types.NodeIP (NodeIP (..))


--------------------------------------------------------------------------------
-- Network Effect
--------------------------------------------------------------------------------

-- | Effect for managing peer connections.
--
-- Provides operations to connect to peers, disconnect, and check connection status.
data NodeToNode :: Effect where
    ConnectToPeer :: Config m -> NodeToNode m Void


-- Generate smart constructors using Template Haskell
makeEffect ''NodeToNode


--------------------------------------------------------------------------------
-- Effect Handler
--------------------------------------------------------------------------------

-- | Run the NodeToNode effect with real implementation.
--
-- This handler establishes actual network connections and spawns protocol threads.
runNodeToNode
    :: ( Clock :> es
       , Concurrent :> es
       , Error Text :> es
       , IOE :> es
       , Log :> es
       , Pub :> es
       , Reader Env :> es
       , State HoardState :> es
       )
    => Eff (NodeToNode : es) a
    -> Eff es a
runNodeToNode =
    interpret $ \env -> \case
        ConnectToPeer conf -> localUnlift env concStrat \unlift ->
            connectToPeerImpl $ hoistConfig unlift conf


hoistConfig :: (forall x. Eff localEs x -> Eff es x) -> Config (Eff localEs) -> Config (Eff es)
hoistConfig unlift conf =
    Config
        { awaitBlockFetchRequests = unlift conf.awaitBlockFetchRequests
        , emitFetchedHeader = unlift . conf.emitFetchedHeader
        , emitFetchedBlock = unlift . conf.emitFetchedBlock
        , peer = conf.peer
        }


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
       , Concurrent :> es
       , Error Text :> es
       , IOE :> es
       , Log :> es
       , Pub :> es
       , Reader Env :> es
       , State HoardState :> es
       )
    => Config (Eff es)
    -> Eff es Void
connectToPeerImpl conf = do
    env <- ask
    let envConf = env.config
        protocolInfo = envConf.protocolInfo
        ioManager = env.handles.ioManager
    let addr = IP.toSockAddr (getNodeIP conf.peer.address.host, fromIntegral conf.peer.address.port)
    -- Create connection using ouroboros-network
    Log.debug $ "Attempting connection to " <> show addr

    Log.debug "Creating snocket..."
    let snocket = socketSnocket ioManager

    -- Load protocol info and create codecs
    Log.debug "Loading protocol configuration..."
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

    -- Helper function to create application for a specific version
    let strat = ConcUnlift Persistent Unlimited
        mkVersionedApp (unlift :: forall x. Eff es x -> IO x) nodeVersion blockVersion =
            let codecs =
                    hoistCodecs liftIO $
                        defaultCodecs
                            codecConfig
                            blockVersion
                            encodeRemoteAddress
                            (\v -> decodeRemoteAddress v)
                            nodeVersion
            in  mkApplication unlift envConf conf codecs conf.peer

    -- Create versions for negotiation - offer all supported versions
    Log.debug "Creating protocol versions..."
    let mkVersions (unlift :: forall x. Eff es x -> IO x) version blockVersion =
            simpleSingletonVersions
                version
                versionData
                (\_ -> mkVersionedApp unlift version blockVersion)

    -- Create versions for all supported protocol versions
    let versions (unlift :: forall x. Eff es x -> IO x) =
            combineVersions
                [ mkVersions unlift nodeVersion blockVersion
                | (nodeVersion, blockVersion) <- Map.toList supportedVersions
                ]

    Log.debug "Codecs created successfully"
    adhocTracers <- withEffToIO strat $ \unlift ->
        pure $
            nullNetworkConnectTracers
                { nctHandshakeTracer = (("[NodeToNode] " <>) . show) >$< Log.asTracer unlift Log.DEBUG
                }

    -- Connect to the peer
    Log.debug "Calling connectTo..."
    result <- do
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
            throwError $ "Failed to connect to peer " <> show conf.peer <> ": " <> show err
        Right (Left ()) -> do
            throwError "Connection closed unexpectedly"
        Right (Right _) -> do
            -- This shouldn't happen with InitiatorOnly mode
            throwError "Unexpected responder mode result"


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
       , Log :> es
       , Pub :> es
       , Reader Env :> es
       , State HoardState :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Env.Config
    -> Config (Eff es)
    -> CardanoCodecs
    -> Peer
    -> OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void
mkApplication unlift envConf conf codecs peer =
    OuroborosApplication
        [ ChainSync.miniProtocol unlift envConf conf codecs peer
        , -- BlockFetch mini-protocol
          MiniProtocol
            { miniProtocolNum = blockFetchMiniProtocolNum
            , miniProtocolLimits = envConf.miniProtocolConfig.blockFetch
            , miniProtocolStart = StartEagerly
            , miniProtocolRun = InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer $ \_ ->
                let codec = cBlockFetchCodec codecs
                    client = blockFetchClientImpl unlift conf peer
                    tracer = (("[BlockFetch tracer] " <>) . show) >$< Log.asTracer unlift Log.DEBUG
                    wrappedPeer = Peer.Effect $ unlift $ withExceptionLogging "BlockFetch" $ do
                        Log.debug "BlockFetch protocol started"
                        pure $ blockFetchClientPeer client
                in  (tracer, codec, wrappedPeer)
            }
        , -- KeepAlive mini-protocol
          MiniProtocol
            { miniProtocolNum = keepAliveMiniProtocolNum
            , miniProtocolLimits = envConf.miniProtocolConfig.keepAlive
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
                                tracer = contramap (("[KeepAlive] " <>) . show) $ Log.asTracer unlift Log.DEBUG
                            in  (tracer, codec, wrappedPeer)
            }
        , -- PeerSharing mini-protocol
          MiniProtocol
            { miniProtocolNum = peerSharingMiniProtocolNum
            , miniProtocolLimits = envConf.miniProtocolConfig.peerSharing
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
                                    publish $ PeerSharingStarted {peer, timestamp}
                                    Log.debug "PeerSharing: Published PeerSharingStarted event"
                                    Log.debug "PeerSharing: About to run peer protocol..."
                                    pure (peerSharingClientPeer client)
                                tracer = contramap (("[PeerSharing] " <>) . show) $ Log.asTracer unlift Log.DEBUG
                            in  (tracer, codec, wrappedPeer)
            }
        , -- TxSubmission mini-protocol (stub - runs forever to avoid terminating)
          MiniProtocol
            { miniProtocolNum = txSubmissionMiniProtocolNum
            , miniProtocolLimits = envConf.miniProtocolConfig.txSubmission
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
    -> Peer
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
                { peer
                , timestamp
                , peerAddresses = S.fromList $ mapMaybe sockAddrToPeerAddress peerAddrs
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
     . ( Clock :> es
       , Concurrent :> es
       , Log :> es
       , Pub :> es
       , Reader Env :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Config (Eff es)
    -> Peer
    -> BlockFetch.BlockFetchClient CardanoBlock CardanoPoint IO ()
blockFetchClientImpl unlift conf peer =
    BlockFetch.BlockFetchClient $ unlift $ do
        timestamp <- Clock.currentTime
        publish $ BlockFetchStarted {peer, timestamp}
        Log.debug "BlockFetch: Published BlockFetchStarted event"
        Log.debug "BlockFetch: Starting client, awaiting block download requests"
        awaitMessage
  where
    awaitMessage :: Eff es (BlockFetch.BlockFetchRequest CardanoBlock CardanoPoint IO ())
    awaitMessage = do
        qSem <- asks $ (.config.blockFetchQSem)
        waitQSem qSem
        reqs <- conf.awaitBlockFetchRequests
        Log.info $ "BlockFetch: Received " <> show (length reqs) <> " block fetch requests"
        let points = headerPoint . (.header) <$> reqs
            start = minimum points
            end = maximum points
        pure
            $ BlockFetch.SendMsgRequestRange
                (BlockFetch.ChainRange start end)
                (handleResponse reqs)
            $ blockFetchClientImpl unlift conf peer

    handleResponse reqs =
        BlockFetch.BlockFetchResponse
            { handleStartBatch =
                pure $ blockReceiver 0
            , handleNoBlocks = unlift $ do
                qSem <- asks $ (.blockFetchQSem) . (.config)
                signalQSem qSem
                timestamp <- Clock.currentTime
                for_ reqs \req ->
                    publish $
                        BlockFetchFailed
                            { peer
                            , timestamp
                            , header = req.header
                            , errorMessage = "No blocks for point"
                            }
            }

    blockReceiver blockCount =
        BlockFetch.BlockFetchReceiver
            { handleBlock = \block -> unlift $ do
                timestamp <- Clock.currentTime
                let event =
                        BlockReceived
                            { peer
                            , timestamp
                            , block
                            }
                conf.emitFetchedBlock event
                publish event
                pure $ blockReceiver $ blockCount + 1
            , handleBatchDone = unlift $ do
                qSem <- asks $ (.config.blockFetchQSem)
                signalQSem qSem
                timestamp <- Clock.currentTime
                publish $
                    BlockBatchCompleted
                        { peer
                        , timestamp
                        , blockCount
                        }
            }


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
