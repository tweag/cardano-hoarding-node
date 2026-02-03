module Hoard.KeepAlive.NodeToNode (miniProtocol, client) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Network.Mux (MiniProtocolLimits (..), StartOnDemandOrEagerly (..))
import Network.TypedProtocol.Peer.Client qualified as Peer
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , RunMiniProtocol (..)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.NodeToNode
    ( keepAliveMiniProtocolNum
    )
import Ouroboros.Network.Protocol.KeepAlive.Client (KeepAliveClient (..), KeepAliveClientSt (..), keepAliveClientPeer)
import Ouroboros.Network.Protocol.KeepAlive.Type (Cookie (..))
import Prelude hiding (Reader, State, asks, evalState, gets)

import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.KeepAlive.Config (Config (..))
import Hoard.Types.Cardano (CardanoCodecs, CardanoMiniProtocol)


miniProtocol
    :: forall es
     . ( Concurrent :> es
       , Log :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Config
    -> CardanoCodecs
    -> CardanoMiniProtocol
miniProtocol unlift' conf codecs =
    MiniProtocol
        { miniProtocolNum = keepAliveMiniProtocolNum
        , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
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
                                        pure (keepAliveClientPeer $ client unlift conf)
                            tracer = show >$< Log.asTracer (unlift . Log.withNamespace "Tracer") Log.DEBUG
                        in  (tracer, codec, wrappedPeer)
        }
  where
    unlift :: forall x. Eff es x -> IO x
    unlift = unlift' . Log.withNamespace "KeepAlive"


-- | KeepAlive client implementation.
--
-- This client sends periodic keepalive messages to maintain the connection
-- and detect network failures. It sends a message immediately, then waits
-- for the configured interval before sending the next one.
client
    :: (Log :> es, Concurrent :> es)
    => (forall x. Eff es x -> IO x)
    -> Config
    -> KeepAliveClient IO ()
client unlift conf = KeepAliveClient sendFirst
  where
    sendFirst = unlift do
        Log.debug "Sending first keepalive message"
        pure $ SendMsgKeepAlive (Cookie 42) sendNext

    sendNext = unlift do
        Log.debug $ "Response received, waiting " <> show (conf.intervalMicroseconds `div` 1_000_000) <> "s before next message"
        threadDelay conf.intervalMicroseconds
        Log.debug "Sending keepalive message"
        pure $ SendMsgKeepAlive (Cookie 42) sendNext
