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
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask)
import Effectful.State.Static.Shared (State, evalState)
import Effectful.TH (makeEffect)
import Effectful.Timeout (Timeout)
import Network.Mux (Mode (..), StartOnDemandOrEagerly (..))
import Network.Socket (SockAddr)
import Ouroboros.Consensus.Config (configBlock, configCodec)
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import Ouroboros.Consensus.Network.NodeToNode (defaultCodecs)
import Ouroboros.Consensus.Node.NetworkProtocolVersion (supportedNodeToNodeVersions)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Network.Diffusion.Configuration (PeerSharing (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolCb (..)
    , MiniProtocolLimits (..)
    , OuroborosApplication (..)
    , OuroborosApplicationWithMinimalCtx
    , RunMiniProtocol (..)
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
    , txSubmissionMiniProtocolNum
    )
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress, encodeRemoteAddress)
import Ouroboros.Network.Snocket (socketSnocket)
import Prelude hiding (Reader, State, ask, asks, evalState, get, gets)

import Hoard.BlockFetch.NodeToNode qualified as BlockFetch
import Hoard.BlockFetch.State qualified as BlockFetch
import Hoard.ChainSync.NodeToNode qualified as ChainSync
import Hoard.Control.Exception (withExceptionLogging)
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
import Hoard.Types.Environment (CardanoProtocolsConfig (..), Env, TxSubmissionConfig (..))
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
    ConnectToPeer :: Peer -> NodeToNode m Void


-- Generate smart constructors using Template Haskell
makeEffect ''NodeToNode


--------------------------------------------------------------------------------
-- Effect Handler
--------------------------------------------------------------------------------

-- | Run the NodeToNode effect with real implementation.
--
-- This handler establishes actual network connections and spawns protocol threads.
runNodeToNode
    :: ( Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Error Text :> es
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
    interpret $ \_ -> \case
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

            bfStatus <- BlockFetch.mkStatus

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
                    in  mkApplication (unlift . evalState bfStatus) env codecs peer

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
                    throwError $ "Failed to connect to peer " <> show peer <> ": " <> show err
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
    :: ( Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , Pub :> es
       , State HoardState :> es
       , State BlockFetch.Status :> es
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
        , -- TxSubmission mini-protocol (stub - runs forever to avoid terminating)
          MiniProtocol
            { miniProtocolNum = txSubmissionMiniProtocolNum
            , miniProtocolLimits = MiniProtocolLimits env.config.cardanoProtocols.txSubmission.maximumIngressQueue
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
