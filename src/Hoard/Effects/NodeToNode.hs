-- |
-- Module: Hoard.Effects.NodeToNode
-- Description: Effect for managing peer connections
--
-- This effect provides high-level operations for connecting to Cardano peers
-- and managing the node-to-node protocol communication.
module Hoard.Effects.NodeToNode
    ( -- * Effect
      NodeToNode
    , connectToPeer
    , ConnectToError (..)

      -- * Interpreter
    , runNodeToNode
    ) where

import Cardano.Api ()
import Data.Time (NominalDiffTime)
import Effectful (Eff, Effect, IOE, Limit (..), Persistence (..), UnliftStrategy (..), withEffToIO, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Exception (Handler (..), IOException, catches, throwIO)
import Effectful.Reader.Static (Reader, ask)
import Effectful.State.Static.Shared (State)
import Effectful.TH (makeEffect)
import Effectful.Timeout (Timeout)
import GHC.IO.Exception (IOErrorType (..), IOException (..), ioError, userError)
import Network.Mux (Mode (..))
import Network.Socket (SockAddr, Socket)
import Ouroboros.Consensus.Config (configBlock, configCodec)
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import Ouroboros.Consensus.Network.NodeToNode (defaultCodecs)
import Ouroboros.Consensus.Node.NetworkProtocolVersion (HasNetworkProtocolVersion (..), supportedNodeToNodeVersions)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Network.Diffusion.Configuration (PeerSharing (..))
import Ouroboros.Network.Mux
    ( OuroborosApplication (..)
    , OuroborosApplicationWithMinimalCtx
    )
import Ouroboros.Network.NodeToNode
    ( DiffusionMode (..)
    , NetworkConnectTracers (..)
    , NodeToNodeVersion
    , NodeToNodeVersionData (..)
    , ProtocolLimitFailure
    , Versions
    , combineVersions
    , connectTo
    , networkMagic
    , nullNetworkConnectTracers
    , simpleSingletonVersions
    )
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress, encodeRemoteAddress)
import Ouroboros.Network.Protocol.Handshake (HandshakeProtocolError (..))
import Ouroboros.Network.Snocket (Snocket (..), socketSnocket)
import Prelude hiding (Reader, State, ask, asks, evalState, get, gets)

import Data.ByteString.Lazy qualified as LBS
import Data.IP qualified as IP
import Data.Map.Strict qualified as Map
import Network.Mux.Trace qualified as Mux
import System.Timeout qualified as Timeout

import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Tracing (SpanStatus (..), Tracing, addAttribute, addEvent, asTracer, setStatus, withSpan)
import Hoard.Effects.NodeToNode.Codecs (hoistCodecs)
import Hoard.Effects.NodeToNode.Config (Config (..), ProtocolsConfig (..))
import Hoard.Effects.NodeToNode.KeepAlive (KeepAlivePing)
import Hoard.Effects.Publishing (Pub, Sub)
import Hoard.Events.ChainSync (ChainSyncIntersectionFound, ChainSyncStarted, RollBackward)
import Hoard.Events.PeerSharing (PeerSharingStarted, PeersReceived)
import Hoard.Types.Cardano (CardanoBlock, CardanoCodecs)
import Hoard.Types.Environment (Env)
import Hoard.Types.HoardState (HoardState (..))
import Hoard.Types.NodeIP (NodeIP (..))

import Hoard.Effects.NodeToNode.BlockFetch qualified as NodeToNode.BlockFetch
import Hoard.Effects.NodeToNode.ChainSync qualified as NodeToNode.ChainSync
import Hoard.Effects.NodeToNode.KeepAlive qualified as NodeToNode.KeepAlive
import Hoard.Effects.NodeToNode.PeerSharing qualified as NodeToNode.PeerSharing
import Hoard.Events.BlockFetch qualified as BlockFetch
import Hoard.Events.ChainSync qualified as ChainSync
import Hoard.Types.Environment qualified as Env


--------------------------------------------------------------------------------
-- Network Effect
--------------------------------------------------------------------------------

-- | Effect for managing peer connections.
--
-- Provides operations to connect to peers, disconnect, and check connection status.
data NodeToNode :: Effect where
    ConnectToPeer :: Peer -> NodeToNode m ConnectToError


newtype ConnectToError = ConnectToError {getConnectToError :: Text}


-- Generate smart constructors using Template Haskell
makeEffect ''NodeToNode


--------------------------------------------------------------------------------
-- Effect Handler
--------------------------------------------------------------------------------

-- | Run the NodeToNode effect with real implementation.
--
-- This handler establishes actual network connections and spawns protocol threads.
runNodeToNode
    :: forall es a
     . ( Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Metrics :> es
       , Pub BlockFetch.BatchCompleted :> es
       , Pub BlockFetch.BlockReceived :> es
       , Pub BlockFetch.RequestFailed :> es
       , Pub BlockFetch.RequestStarted :> es
       , Pub ChainSync.HeaderReceived :> es
       , Pub ChainSyncIntersectionFound :> es
       , Pub ChainSyncStarted :> es
       , Pub KeepAlivePing :> es
       , Pub PeerSharingStarted :> es
       , Pub PeersReceived :> es
       , Pub RollBackward :> es
       , Reader Env :> es
       , Reader ProtocolsConfig :> es
       , State HoardState :> es
       , Sub BlockFetch.Request :> es
       , Timeout :> es
       , Tracing :> es
       )
    => Eff (NodeToNode : es) a
    -> Eff es a
runNodeToNode =
    interpret_ \case
        ConnectToPeer peer -> withSpan "node_to_node.connect_to_peer" $ do
            env <- ask
            let protocolInfo = env.config.protocolInfo
                ioManager = env.handles.ioManager
            let addr = IP.toSockAddr (getNodeIP peer.address.host, fromIntegral peer.address.port)

            addAttribute "peer.id" (show peer.id)
            addAttribute "peer.host" (show peer.address.host)
            addAttribute "peer.port" (show peer.address.port)
            addAttribute "peer.address" (show addr)
            addEvent "connection_attempt" [("address", show addr)]

            addEvent "creating_snocket" []
            let connectionTimeout = env.config.nodeToNode.connectionTimeoutSeconds
            let snocket = withConnectionTimeout connectionTimeout (socketSnocket ioManager)

            -- Load protocol info and create codecs
            addEvent "loading_protocol_config" []
            let codecConfig = configCodec (pInfoConfig protocolInfo)
            let networkMagic = getNetworkMagic (configBlock (pInfoConfig protocolInfo))

            -- Get all supported versions
            let supportedVersions = supportedNodeToNodeVersions (Proxy :: Proxy CardanoBlock)

            addEvent "creating_codecs" []

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
                mkVersionedApp :: (forall x. Eff es x -> IO x) -> NodeToNodeVersion -> BlockNodeToNodeVersion CardanoBlock -> Eff es (OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void)
                mkVersionedApp (unlift :: forall x. Eff es x -> IO x) nodeVersion blockVersion =
                    let codecs =
                            hoistCodecs liftIO
                                $ defaultCodecs
                                    codecConfig
                                    blockVersion
                                    encodeRemoteAddress
                                    (\v -> decodeRemoteAddress v)
                                    nodeVersion
                    in  mkApplication unlift env codecs peer

            -- Create versions for negotiation - offer all supported versions
            addEvent "creating_protocol_versions" []
            let
                mkVersions :: (forall x. Eff es x -> IO x) -> NodeToNodeVersion -> BlockNodeToNodeVersion CardanoBlock -> Eff es (Versions NodeToNodeVersion NodeToNodeVersionData (OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void))
                mkVersions unlift version blockVersion = do
                    app <- mkVersionedApp unlift version blockVersion
                    pure
                        $ simpleSingletonVersions
                            version
                            versionData
                            (\_ -> app)

            -- Create versions for all supported protocol versions
            let
                versions :: (forall x. Eff es x -> IO x) -> Eff es (Versions NodeToNodeVersion NodeToNodeVersionData (OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void))
                versions unlift = do
                    vs <-
                        traverse (uncurry $ mkVersions unlift)
                            $ Map.toList supportedVersions
                    pure $ combineVersions vs

            addEvent "codecs_created" []

            flip
                catches
                [ Handler $ handleIOException peer
                , Handler $ handleMuxError peer
                , Handler $ handleHandshakeProtocolError peer
                , Handler $ handleProtocolLimitFailure peer
                ]
                do
                    -- Connect to the peer
                    addEvent "initiating_connection" []
                    adhocTracers <- withEffToIO strat $ \unlift ->
                        pure
                            $ nullNetworkConnectTracers
                                { nctHandshakeTracer = show >$< asTracer unlift "node_to_node.handshake"
                                }
                    result <- withEffToIO strat $ \unlift -> do
                        vs <- unlift $ versions unlift
                        connectTo
                            snocket
                            adhocTracers
                            vs
                            Nothing -- No local address binding
                            addr

                    addEvent "connection_returned" []

                    case result of
                        Left err -> do
                            let errMsg = "Failed to connect to peer " <> show peer <> ": " <> show err
                            addEvent "connection_failed" [("error", show err)]
                            setStatus $ Error errMsg
                            pure $ ConnectToError errMsg
                        Right (Left ()) -> do
                            addEvent "connection_closed" []
                            setStatus $ Error "Connection closed unexpectedly"
                            pure $ ConnectToError "Connection closed unexpectedly"
                        Right (Right v) -> do
                            -- This can't happen with InitiatorOnly mode
                            absurd v
  where
    handleIOException :: Peer -> IOException -> Eff es ConnectToError
    handleIOException peer e = do
        let errMsg = case e.ioe_type of
                NoSuchThing ->
                    "Could not create a socket to connect to peer: desc (" <> toText e.ioe_description <> "), peer (" <> show peer.address <> ")"
                ResourceVanished ->
                    "Connection dropped: desc (" <> toText e.ioe_description <> "), peer (" <> show peer.address <> ")"
                TimeExpired ->
                    "Connection timed out: peer (" <> show peer.address <> ")"
                _ ->
                    "Unknown IOException in connectTo: error (" <> show e <> "), peer (" <> show peer.address <> ")"
        case e.ioe_type of
            NoSuchThing -> do
                addEvent "io_exception" [("type", "NoSuchThing"), ("error", toText e.ioe_description)]
                setStatus $ Error errMsg
                pure $ ConnectToError errMsg
            ResourceVanished -> do
                addEvent "io_exception" [("type", "ResourceVanished"), ("error", toText e.ioe_description)]
                setStatus $ Error errMsg
                pure $ ConnectToError errMsg
            TimeExpired -> do
                addEvent "io_exception" [("type", "TimeExpired")]
                setStatus $ Error errMsg
                pure $ ConnectToError errMsg
            _ -> do
                addEvent "io_exception" [("type", "Unknown"), ("error", show e)]
                setStatus $ Error errMsg
                throwIO $ userError $ toString errMsg

    handleMuxError :: Peer -> Mux.Error -> Eff es ConnectToError
    handleMuxError peer = \case
        Mux.IOException e _msg ->
            handleIOException peer e
        Mux.BearerClosed reason -> do
            let errMsg = "Disconnected due to the other party closing the socket: " <> toText reason <> ", " <> show peer.address
            addEvent "mux_error" [("type", "BearerClosed"), ("reason", toText reason)]
            setStatus $ Error errMsg
            pure $ ConnectToError errMsg
        e -> do
            let errMsg = "Disconnected due to unknown ouroboros error: " <> show e
            addEvent "mux_error" [("type", "Unknown"), ("error", show e)]
            setStatus $ Error errMsg
            throwIO $ userError $ toString errMsg

    handleHandshakeProtocolError :: Peer -> HandshakeProtocolError NodeToNodeVersion -> Eff es ConnectToError
    handleHandshakeProtocolError peer e = do
        let errMsg = "Handshake failed: reason (" <> show e <> "), peer (" <> show peer.address <> ")"
        addEvent "handshake_error" [("error", show e)]
        setStatus $ Error errMsg
        pure $ ConnectToError errMsg

    handleProtocolLimitFailure :: Peer -> ProtocolLimitFailure -> Eff es ConnectToError
    handleProtocolLimitFailure peer e = do
        let errMsg = "Protocol limit exceeded: " <> show e <> ", peer (" <> show peer.address <> ")"
        addEvent "protocol_limit_error" [("error", show e)]
        setStatus $ Error errMsg
        pure $ ConnectToError errMsg


--------------------------------------------------------------------------------
-- Mini-Protocol Application
--------------------------------------------------------------------------------

-- | Create the Ouroboros application with all mini-protocols.
--
-- This bundles together ChainSync, BlockFetch, and KeepAlive protocols into
-- an application that runs over the multiplexed connection.
mkApplication
    :: ( Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Metrics :> es
       , Pub BlockFetch.BatchCompleted :> es
       , Pub BlockFetch.BlockReceived :> es
       , Pub BlockFetch.RequestFailed :> es
       , Pub BlockFetch.RequestStarted :> es
       , Pub ChainSync.HeaderReceived :> es
       , Pub ChainSyncIntersectionFound :> es
       , Pub ChainSyncStarted :> es
       , Pub KeepAlivePing :> es
       , Pub PeerSharingStarted :> es
       , Pub PeersReceived :> es
       , Pub RollBackward :> es
       , Reader ProtocolsConfig :> es
       , State HoardState :> es
       , Sub BlockFetch.Request :> es
       , Timeout :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Env
    -> CardanoCodecs
    -> Peer
    -> Eff es (OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void)
mkApplication unlift _env codecs peer = do
    conf <- ask @ProtocolsConfig
    blockFetch <- NodeToNode.BlockFetch.miniProtocol conf.blockFetch unlift codecs peer
    chainSync <- NodeToNode.ChainSync.miniProtocol conf.chainSync unlift codecs peer
    keepAlive <- NodeToNode.KeepAlive.miniProtocol conf.keepAlive unlift codecs peer
    peerSharing <- NodeToNode.PeerSharing.miniProtocol conf.peerSharing unlift codecs peer
    pure
        $ OuroborosApplication
            [ chainSync
            , blockFetch
            , keepAlive
            , peerSharing
            ]


--------------------------------------------------------------------------------
-- Snocket with Connection Timeout
--------------------------------------------------------------------------------

-- | Wrap a snocket with a connection timeout.
--
-- This wraps only the 'connect' operation, so it only affects the TCP
-- handshake phase. Once the connection is established, the timeout no longer
-- applies.
withConnectionTimeout
    :: NominalDiffTime
    -- ^ Timeout in seconds for the TCP connection handshake
    -> Snocket IO Socket SockAddr
    -> Snocket IO Socket SockAddr
withConnectionTimeout timeoutDurationSeconds snocket =
    snocket {connect = timedConnect}
  where
    timedConnect socket addr = do
        let timeoutMicros = round (timeoutDurationSeconds * 1_000_000)
        result <- Timeout.timeout timeoutMicros (connect snocket socket addr)
        case result of
            Nothing ->
                ioError
                    $ IOError
                        { ioe_handle = Nothing
                        , ioe_type = TimeExpired
                        , ioe_location = "connect"
                        , ioe_description = "Connection timed out"
                        , ioe_errno = Nothing
                        , ioe_filename = Nothing
                        }
            Just () -> pure ()
