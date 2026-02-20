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
import Data.Traversable (for)
import Effectful (Eff, Effect, IOE, Limit (..), Persistence (..), UnliftStrategy (..), withEffToIO, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Exception (Handler (..), IOException, catches)
import Effectful.Reader.Static (Reader, ask)
import Effectful.State.Static.Shared (State)
import Effectful.TH (makeEffect)
import Effectful.Timeout (Timeout)
import GHC.IO.Exception (IOErrorType (..), IOException (..), ioError)
import Network.Mux (Mode (..))
import Network.Socket (SockAddr, Socket)
import Ouroboros.Consensus.Config (TopLevelConfig (..))
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import Ouroboros.Consensus.Network.NodeToNode (defaultCodecs)
import Ouroboros.Consensus.Node (ProtocolInfo (..))
import Ouroboros.Consensus.Node.NetworkProtocolVersion (supportedNodeToNodeVersions)
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
import Hoard.Effects.Monitoring.Tracing (SpanStatus (..), Tracing, addAttribute, addEvent, setStatus, withSpan)
import Hoard.Effects.NodeToNode.Codecs (hoistCodecs)
import Hoard.Effects.NodeToNode.Config (NodeToNodeConfig (..), ProtocolsConfig (..))
import Hoard.Effects.Publishing (Pub, Sub)
import Hoard.Effects.UUID (GenUUID)
import Hoard.Events.ChainSync (ChainSyncIntersectionFound, ChainSyncStarted, RollBackward)
import Hoard.Events.KeepAlive (KeepAlivePing)
import Hoard.Events.PeerSharing (PeerSharingStarted, PeersReceived)
import Hoard.Types.Cardano (CardanoBlock, CardanoCodecs)
import Hoard.Types.Environment (Config (..), Env (..), Handles (..))
import Hoard.Types.HoardState (HoardState (..))
import Hoard.Types.NodeIP (NodeIP (..))

import Hoard.Effects.Monitoring.Tracing qualified as Tracing
import Hoard.Effects.NodeToNode.BlockFetch qualified as NodeToNode.BlockFetch
import Hoard.Effects.NodeToNode.ChainSync qualified as NodeToNode.ChainSync
import Hoard.Effects.NodeToNode.KeepAlive qualified as NodeToNode.KeepAlive
import Hoard.Effects.NodeToNode.PeerSharing qualified as NodeToNode.PeerSharing
import Hoard.Events.BlockFetch qualified as BlockFetch
import Hoard.Events.ChainSync qualified as ChainSync


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
       , GenUUID :> es
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
       , Reader NodeToNodeConfig :> es
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
        ConnectToPeer peer -> withSpan "node_to_node.connect_to_peer" do
            addAttribute "peer.id" (show peer.id)
            addAttribute "peer.address" (show peer.address)

            env <- ask @Env
            nodeToNode <- ask @NodeToNodeConfig
            let addr = IP.toSockAddr (peer.address.host.getNodeIP, fromIntegral peer.address.port)
                snocket =
                    withConnectionTimeout nodeToNode.connectionTimeoutSeconds
                        $ socketSnocket env.handles.ioManager
                networkMagic = getNetworkMagic $ env.config.protocolInfo.pInfoConfig.topLevelConfigBlock
                supportedVersions = Map.toList $ supportedNodeToNodeVersions (Proxy :: Proxy CardanoBlock)

                mkCodecs version blockVersion =
                    hoistCodecs liftIO
                        $ defaultCodecs
                            env.config.protocolInfo.pInfoConfig.topLevelConfigCodec
                            blockVersion
                            encodeRemoteAddress
                            (\v -> decodeRemoteAddress v)
                            version

                -- Create version data for handshake
                versionData =
                    NodeToNodeVersionData
                        { networkMagic
                        , diffusionMode = InitiatorOnlyDiffusionMode
                        , peerSharing = PeerSharingEnabled
                        , query = False
                        }

            -- Create versions for all supported protocol versions
            versions <- withSpan "create_protocol_versions" do
                vs <-
                    for supportedVersions \(version, blockVersion) ->
                        withSpan "create_protocol_version" do
                            addAttribute "node_to_node_version" $ show version
                            addAttribute "block_node_to_node_version" $ show blockVersion

                            app <-
                                withSpan "create_application"
                                    $ mkApplication (mkCodecs version blockVersion) peer

                            pure
                                $ simpleSingletonVersions
                                    version
                                    versionData
                                    (\_ -> app)
                pure $ combineVersions vs

            -- Connect to peer and handle expected exceptions and errors
            flip
                catches
                [ Handler $ handleIOException peer
                , Handler $ handleMuxError peer
                , Handler $ handleHandshakeProtocolError peer
                , Handler $ handleProtocolLimitFailure peer
                ]
                $ withSpan "connection" do
                    result <- withEffToIO (ConcUnlift Persistent Unlimited) \unlift -> do
                        let tracers =
                                nullNetworkConnectTracers
                                    { nctHandshakeTracer = show >$< Tracing.asTracer unlift "node_to_node.handshake"
                                    }
                        connectTo snocket tracers versions Nothing addr

                    handleResult peer result
  where
    handleResult :: Peer -> Either SomeException (Either () Void) -> Eff es ConnectToError
    handleResult peer = \case
        Left err -> do
            addEvent "connection_failed" [("error", show err)]
            let errMsg = "Failed to connect to peer " <> show peer <> ": " <> show err
            setStatus $ Error errMsg
            pure $ ConnectToError errMsg
        Right (Left ()) -> do
            addEvent "connection_closed" []
            setStatus $ Error "Connection closed unexpectedly"
            pure $ ConnectToError "Connection closed unexpectedly"
        Right (Right v) -> do
            -- This can't happen with InitiatorOnly mode
            absurd v

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
                pure $ ConnectToError errMsg

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
            pure $ ConnectToError errMsg

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
       , GenUUID :> es
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
       , Reader ProtocolsConfig :> es
       , State HoardState :> es
       , Sub BlockFetch.Request :> es
       , Timeout :> es
       , Tracing :> es
       )
    => CardanoCodecs
    -> Peer
    -> Eff es (OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void)
mkApplication codecs peer = do
    conf <- ask @ProtocolsConfig
    withEffToIO (ConcUnlift Persistent Unlimited) \unlift ->
        pure
            $ OuroborosApplication
                [ NodeToNode.BlockFetch.miniProtocol conf.blockFetch unlift codecs peer
                , NodeToNode.ChainSync.miniProtocol conf.chainSync unlift codecs peer
                , NodeToNode.KeepAlive.miniProtocol conf.keepAlive unlift codecs peer
                , NodeToNode.PeerSharing.miniProtocol conf.peerSharing unlift codecs peer
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
