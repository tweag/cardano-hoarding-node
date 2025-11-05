{- HLINT ignore "Use newTVarIO" -}
module Hoard.Effects.Network.Diffusion (runDiffusion) where

import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (..), StrictTVar, newTVar)
import Control.Exception (IOException, fromException)
import Control.Tracer (Tracer, nullTracer)
import Data.Time (DiffTime, secondsToDiffTime)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Effectful (Eff, IOE, liftIO, (:>))
import Network.DNS (Resolver)
import Network.Mux.Bearer (MakeBearer (..))
import Network.Socket (SockAddr (..), Socket, tupleToHostAddress)
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Diffusion (Arguments (daInstallSigUSR1Handler), DiffusionTracer, noBindForkPolicy)
import Ouroboros.Network.Diffusion.Configuration (PeerSelectionTargets (..), PeerSharing (..))
import Ouroboros.Network.Diffusion.Policies (simplePeerSelectionPolicy)
import Ouroboros.Network.Driver.Limits (ProtocolTimeLimits (ProtocolTimeLimits))
import Ouroboros.Network.ExitPolicy (RepromoteDelay (RepromoteDelay))
import Ouroboros.Network.IOManager (IOManager, IOManagerError)
import Ouroboros.Network.NodeToClient (LocalAddress, LocalSocket, NodeToClientVersion, NodeToClientVersionData, nodeToClientCodecCBORTerm)
import Ouroboros.Network.NodeToNode
    ( AcceptedConnectionsLimit (..)
    , DiffusionMode (..)
    , NodeToNodeVersion (..)
    , NodeToNodeVersionData (..)
    , RemoteAddress
    , Versions (..)
    , combineVersions
    , nodeToNodeCodecCBORTerm
    , nodeToNodeHandshakeCodec
    , simpleSingletonVersions
    )
import Ouroboros.Network.PeerSelection
    ( AfterSlot (..)
    , LedgerPeersConsensusInterface (..)
    , PeerMetricsConfiguration (..)
    , UseLedgerPeers (..)
    , newPeerMetric
    , nullPublicExtraPeersAPI
    , peerChurnGovernor
    )
import Ouroboros.Network.PeerSelection.Governor (PeerSelectionGovernorArgs (..), makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSelection.Governor.Types (ExtraGuardedDecisions (..))
import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (Config)
import Ouroboros.Network.PeerSharing (newPeerSharingRegistry)
import Ouroboros.Network.Point (origin)
import Ouroboros.Network.Protocol.Handshake
    ( HandshakeArguments (..)
    , acceptableVersion
    , cborTermVersionDataCodec
    , nodeToClientHandshakeCodec
    , queryVersion
    )
import Ouroboros.Network.RethrowPolicy (ErrorCommand (..), ErrorContext (..), RethrowPolicy (..))
import Ouroboros.Network.Snocket (Accept (..), Snocket (..))
import System.Random (StdGen, mkStdGen)

import Data.Map.Strict qualified as Map
import Network.Mux qualified as Mx
import Ouroboros.Network.ConnectionManager.Types qualified as CM
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.Diffusion.Configuration qualified as Diffusion

import Hoard.Data.Peer (Peer (..), resolvePeerAddress)
import Hoard.Effects.Network.MiniProtocols (mkApplication)
import Hoard.Network.Config (NetworkConfig (..))


runDiffusion
    :: forall es
     . (IOE :> es)
    => IOManager
    -> (forall event. (Typeable event) => event -> IO ())
    -> NetworkConfig
    -> Peer
    -> Eff es Void
runDiffusion ioManager publishIO config peer = do
    dcPublicPeerSelectionVar <- liftIO makePublicPeerSelectionStateVar
    -- TODO: Make this StdGen less deterministically
    (peerSelectionPolicyVar :: StrictTVar (Eff es) StdGen) <- atomically $ newTVar $ mkStdGen 2
    peerMetrics <-
        newPeerMetric
            $ PeerMetricsConfiguration
                { -- TODO: The docs say this _must_ equate to the number of
                  -- headers / blocks which are produced per hour. I think this
                  -- is 1 second per block.
                  maxEntriesToTrack = 3600
                }
    daPeerSharingRegistry <- newPeerSharingRegistry
    peerAddr <- resolvePeerAddress peer

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
                , dcReadPublicRootPeers = pure $ Map.fromList []
                , dcReadLedgerPeerSnapshot = pure Nothing
                , dcReadUseLedgerPeers = pure $ UseLedgerPeers $ After $ SlotNo 10
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
                $ mkApplication peer NodeToNodeV_14
        versionsV15 =
            simpleSingletonVersions
                NodeToNodeV_15
                versionData
                $ mkApplication peer NodeToNodeV_15
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
        applications :: Diffusion.Applications RemoteAddress NodeToNodeVersion NodeToNodeVersionData LocalAddress NodeToClientVersion NodeToClientVersionData (Eff es) ()
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
    interfaces <- fmap unliftInterfaces $ liftIO $ Diffusion.mkInterfaces ioManager nullTracer 0.2
    Diffusion.runM interfaces Diffusion.nullTracers arguments configuration applications


unliftInterfaces :: (IOE :> es) => Diffusion.Interfaces ntnFd ntnAddr ntcFd ntcAddr resolver IO -> Diffusion.Interfaces ntnFd ntnAddr ntcFd ntcAddr resolver (Eff es)
unliftInterfaces interfaces =
    Diffusion.Interfaces
        { diNtnSnocket = liftSnocket interfaces.diNtnSnocket
        , diNtnBearer = liftMakeBearer interfaces.diNtnBearer
        }


liftSnocket :: Snocket IO fd addr -> Snocket (Eff es) fd addr
liftSnocket snocket =
    Snocket
        { getLocalAddr = liftIO . snocket.getLocalAddr
        , getRemoteAddr = liftIO . snocket.getRemoteAddr
        , addrFamily = snocket.addrFamily
        , open = liftIO . snocket.open
        , openToConnect = liftIO . snocket.openToConnect
        , connect = \fd addr -> liftIO $ snocket.connect fd addr
        , bind = \fd addr -> liftIO $ snocket.bind fd addr
        , listen = liftIO . snocket.listen
        , accept = liftIO . fmap liftAccept . liftIO . snocket.accept
        , close = liftIO . snocket.close
        }


liftAccept :: Accept IO fd addr -> Accept (Eff es) fd addr
liftAccept a = Accept $ do
    (accepted, accept) <- liftIO $ runAccept a
    pure (accepted, liftAccept accept)


mkInterfaces
    :: IOManager
    -> Tracer (Eff es) (DiffusionTracer ntnAddr ntcAddr)
    -> DiffTime
    -> Eff
        es
        ( Diffusion.Interfaces
            Socket
            RemoteAddress
            LocalSocket
            LocalAddress
            Resolver
            (Eff es)
        )
mkInterfaces iocp tracer egressPollInterval = do
    diRng <- newStdGen
    diConnStateIdSupply <- atomically $ CM.newConnStateIdSupply Proxy

    -- Clamp the mux egress poll interval to sane values.
    let egressInterval = max 0 $ min 0.200 egressPollInterval

    return
        $ Interfaces
            { diNtnSnocket = Snocket.socketSnocket iocp
            , diNtnBearer = makeSocketBearer' egressInterval
            , diWithBuffer = withReadBufferIO
            , diNtnConfigureSocket = configureSocket
            , diNtnConfigureSystemdSocket =
                configureSystemdSocket
                    (SystemdSocketConfiguration `contramap` tracer)
            , diNtnAddressType = socketAddressType
            , diNtnToPeerAddr = curry IP.toSockAddr
            , diNtcSnocket = Snocket.localSnocket iocp
            , diNtcBearer = makeLocalBearer
            , diNtcGetFileDescriptor = localSocketFileDescriptor
            , diDnsActions = RootPeersDNS.ioDNSActions
            , diRng
            , diConnStateIdSupply
            }
