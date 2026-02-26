module Hoard.Effects.NodeToNode.KeepAlive
    ( miniProtocol
    ) where

import Effectful.Concurrent (Concurrent, threadDelay)
import Network.Mux (MiniProtocolLimits (..), StartOnDemandOrEagerly (..))
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , RunMiniProtocol (..)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.NodeToNode (keepAliveMiniProtocolNum)
import Ouroboros.Network.Protocol.KeepAlive.Client (KeepAliveClient (..), KeepAliveClientSt (..), keepAliveClientPeer)
import Ouroboros.Network.Protocol.KeepAlive.Type (Cookie (..))

import Network.TypedProtocol.Peer.Client qualified as Peer

import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Tracing (Tracing, asTracer)
import Hoard.Effects.NodeToNode.Config (KeepAliveConfig (..))
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Events.KeepAlive (Ping (..))
import Hoard.Types.Cardano (CardanoCodecs, CardanoMiniProtocol)

import Hoard.Effects.Clock qualified as Clock


miniProtocol
    :: forall es
     . ( Clock :> es
       , Concurrent :> es
       , Log :> es
       , Pub Ping :> es
       , Tracing :> es
       )
    => KeepAliveConfig
    -> (forall x. Eff es x -> IO x)
    -> CardanoCodecs
    -> Peer
    -> CardanoMiniProtocol
miniProtocol conf unlift codecs peer =
    MiniProtocol
        { miniProtocolNum = keepAliveMiniProtocolNum
        , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
        , miniProtocolStart = StartEagerly
        , miniProtocolRun =
            InitiatorProtocolOnly
                $ mkMiniProtocolCbFromPeer
                $ \_ ->
                    let codec = cKeepAliveCodec codecs
                        wrappedPeer =
                            Peer.Effect
                                $ unlift
                                $ withExceptionLogging "KeepAlive"
                                $ pure
                                $ keepAliveClientPeer
                                $ client unlift conf peer
                        tracer = show >$< asTracer unlift "keep_alive.protocol_message"
                    in  (tracer, codec, wrappedPeer)
        }


-- | KeepAlive client implementation.
--
-- This client sends periodic keepalive messages to maintain the connection
-- and detect network failures. It sends a message immediately, then waits
-- for the configured interval before sending the next one.
client
    :: ( Clock :> es
       , Concurrent :> es
       , Pub Ping :> es
       )
    => (forall x. Eff es x -> IO x)
    -> KeepAliveConfig
    -> Peer
    -> KeepAliveClient IO ()
client unlift conf peer = KeepAliveClient sendFirst
  where
    sendFirst = unlift do
        timestamp <- Clock.currentTime
        publish Ping {timestamp, peer}
        pure $ SendMsgKeepAlive (Cookie 42) sendNext

    sendNext = unlift do
        threadDelay conf.intervalMicroseconds
        timestamp <- Clock.currentTime
        publish Ping {timestamp, peer}
        pure $ SendMsgKeepAlive (Cookie 42) sendNext
