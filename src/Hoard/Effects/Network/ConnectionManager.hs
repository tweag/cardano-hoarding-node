module Hoard.Effects.Network.ConnectionManager (withConnectionManager) where

import Control.Concurrent (ThreadId)
import Control.Exception (IOException, fromException)
import Control.Tracer (nullTracer)
import Data.Time (secondsToDiffTime)
import Data.Typeable (Proxy (..), Typeable)
import Effectful (Eff, (:>))
import Effectful.Concurrent.STM (Concurrent, atomically)
import Network.Mux (Mode (..), nullTracers)
import Network.Mux.Bearer (makeSocketBearer, withReadBufferIO)
import Network.Socket (SockAddr (..), Socket)
import Ouroboros.Network.ConnectionHandler (MkMuxConnectionHandler (..), classifyHandleError, makeConnectionHandler)
import Ouroboros.Network.ConnectionManager.Types (simplePrunePolicy)
import Ouroboros.Network.Diffusion (noBindForkPolicy, socketAddressType)
import Ouroboros.Network.Diffusion.Configuration (PeerSharing (..))
import Ouroboros.Network.Driver.Limits (ProtocolTimeLimits (ProtocolTimeLimits))
import Ouroboros.Network.IOManager (IOManagerError)
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.MuxMode (InResponderMode (NotInResponderMode))
import Ouroboros.Network.NodeToNode
    ( AcceptedConnectionsLimit (..)
    , DiffusionMode (..)
    , NodeToNodeVersion (..)
    , NodeToNodeVersionData (..)
    , combineVersions
    , nodeToNodeCodecCBORTerm
    , nodeToNodeHandshakeCodec
    , simpleSingletonVersions
    )
import Ouroboros.Network.Protocol.Handshake
    ( HandshakeArguments (..)
    , acceptableVersion
    , cborTermVersionDataCodec
    , queryVersion
    )
import Ouroboros.Network.RethrowPolicy (ErrorCommand (..), ErrorContext (..), RethrowPolicy (..))
import Ouroboros.Network.Snocket (Snocket)
import Ouroboros.Network.Socket (configureSocket)
import System.Random (mkStdGen)

import Network.Mux qualified as Mx
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.State qualified as CM
import Ouroboros.Network.ConnectionManager.Types qualified as CM

import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Network.MiniProtocols (mkApplication)


withConnectionManager
    :: (Concurrent :> es)
    => ThreadId
    -> NetworkMagic
    -> Peer
    -> Snocket IO Socket SockAddr
    -> (forall handle handleError. (Show handleError) => CM.ConnectionManager InitiatorMode Socket SockAddr handle handleError (Eff es) -> Eff es a)
    -> Eff es a
withConnectionManager mainThreadId networkMagic peer snocket action = do
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
            $ mkApplication peer NodeToNodeV_14
    versionsV15 =
        simpleSingletonVersions
            NodeToNodeV_15
            versionData
            $ mkApplication peer NodeToNodeV_15
    versions = combineVersions [versionsV14, versionsV15]
