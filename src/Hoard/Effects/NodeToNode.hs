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

      -- * Interpreter
    , runNodeToNode
    ) where

import Cardano.Api ()
import Data.ByteString.Lazy qualified as LBS
import Data.IP qualified as IP
import Data.Map.Strict qualified as Map
import Effectful (Eff, Effect, IOE, Limit (..), Persistence (..), UnliftStrategy (..), withEffToIO, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Exception (Handler (..), IOException, catches, throwIO)
import Effectful.Reader.Static (Reader, ask)
import Effectful.State.Static.Shared (State)
import Effectful.TH (makeEffect)
import Effectful.Timeout (Timeout)
import Network.Mux (Mode (..))
import Network.Socket (SockAddr)
import Ouroboros.Consensus.Config (configBlock, configCodec)
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import Ouroboros.Consensus.Network.NodeToNode (defaultCodecs)
import Ouroboros.Consensus.Node.NetworkProtocolVersion (supportedNodeToNodeVersions)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Network.Diffusion.Configuration (PeerSharing (..))
import Ouroboros.Network.Mux
    ( OuroborosApplication (..)
    , OuroborosApplicationWithMinimalCtx
    )
import Ouroboros.Network.NodeToNode
    ( DiffusionMode (..)
    , NetworkConnectTracers (..)
    , NodeToNodeVersionData (..)
    , combineVersions
    , connectTo
    , networkMagic
    , nullNetworkConnectTracers
    , simpleSingletonVersions
    )
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress, encodeRemoteAddress)
import Ouroboros.Network.Snocket (socketSnocket)
import Prelude hiding (Reader, State, ask, asks, evalState, get, gets)

import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Hoard.BlockFetch.NodeToNode qualified as BlockFetch
import Hoard.ChainSync.NodeToNode qualified as ChainSync
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode.Codecs (hoistCodecs)
import Hoard.Effects.Publishing (Pub, Sub)
import Hoard.KeepAlive.NodeToNode qualified as KeepAlive
import Hoard.PeerSharing.NodeToNode qualified as PeerSharing
import Hoard.Types.Cardano (CardanoBlock, CardanoCodecs)
import Hoard.Types.Environment (CardanoProtocolsConfig (..), Env)
import Hoard.Types.Environment qualified as Env
import Hoard.Types.HoardState (HoardState (..))
import Hoard.Types.NodeIP (NodeIP (..))
import Network.Mux.Trace qualified as Mux


--------------------------------------------------------------------------------
-- Network Effect
--------------------------------------------------------------------------------

-- | Effect for managing peer connections.
--
-- Provides operations to connect to peers, disconnect, and check connection status.
data NodeToNode :: Effect where
    ConnectToPeer :: Peer -> NodeToNode m ()


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
       , Log :> es
       , Pub :> es
       , Reader Env :> es
       , State HoardState :> es
       , Sub :> es
       , Timeout :> es
       )
    => Eff (NodeToNode : es) a
    -> Eff es a
runNodeToNode =
    interpret_ \case
        ConnectToPeer peer -> do
            env <- ask
            let protocolInfo = env.config.protocolInfo
                ioManager = env.handles.ioManager
            let addr = IP.toSockAddr (getNodeIP peer.address.host, fromIntegral peer.address.port)
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
                    in  mkApplication unlift env codecs peer

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

            flip
                catches
                [ Handler $ handleIOException peer
                , Handler $ handleMuxError peer
                ]
                do
                    -- Connect to the peer
                    Log.debug "Calling connectTo..."
                    result <- withEffToIO strat $ \unlift -> do
                        let adhocTracers =
                                nullNetworkConnectTracers
                                    { nctHandshakeTracer = (("[NodeToNode] " <>) . show) >$< Log.asTracer unlift Log.DEBUG
                                    }
                        connectTo
                            snocket
                            adhocTracers
                            (versions unlift)
                            Nothing -- No local address binding
                            addr

                    Log.debug "connectTo returned!"

                    case result of
                        Left err -> do
                            Log.warn $ "Failed to connect to peer " <> show peer <> ": " <> show err
                        Right (Left ()) -> do
                            Log.warn "Connection closed unexpectedly"
                        Right (Right v) -> do
                            -- This can't happen with InitiatorOnly mode
                            pure $ absurd v
  where
    handleIOException :: Peer -> IOException -> Eff es ()
    handleIOException peer e =
        case e.ioe_type of
            NoSuchThing ->
                Log.warn $ "Could not create a socket to connect to peer: desc (" <> toText e.ioe_description <> "), peer (" <> show peer.address <> ")"
            ResourceVanished ->
                Log.warn $ "Connection dropped: desc (" <> toText e.ioe_description <> "), peer (" <> show peer.address <> ")"
            TimeExpired ->
                Log.warn $ "Connection timed out: peer (" <> show peer.address <> ")"
            _ -> do
                Log.warn $ "Unknown IOException in connectTo: error (" <> show e <> "), peer (" <> show peer.address <> ")"
                throwIO e

    handleMuxError :: Peer -> Mux.Error -> Eff es ()
    handleMuxError peer = \case
        Mux.IOException e _msg ->
            handleIOException peer e
        Mux.BearerClosed reason ->
            Log.warn $ "Disconnected due to the other party closing the socket: " <> toText reason <> ", " <> show peer.address
        e -> do
            Log.warn $ "Disconnected due to unknown ouroboros error: " <> show e
            throwIO e


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
       , Log :> es
       , Pub :> es
       , State HoardState :> es
       , Sub :> es
       , Timeout :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Env
    -> CardanoCodecs
    -> Peer
    -> OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LBS.ByteString IO () Void
mkApplication unlift env codecs peer =
    OuroborosApplication
        [ ChainSync.miniProtocol unlift env.config.cardanoProtocols.chainSync codecs peer
        , BlockFetch.miniProtocol unlift env.config.cardanoProtocols.blockFetch codecs peer
        , KeepAlive.miniProtocol unlift env.config.cardanoProtocols.keepAlive codecs
        , PeerSharing.miniProtocol unlift env.config.cardanoProtocols.peerSharing codecs peer
        ]
