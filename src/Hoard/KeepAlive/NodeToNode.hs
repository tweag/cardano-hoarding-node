module Hoard.KeepAlive.NodeToNode
    ( miniProtocol
    , client
    , KeepAlivePing (..)
    ) where

import Data.Time (UTCTime)
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
import Hoard.Data.Peer (Peer)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Monitoring.Tracing (Tracing, addEvent, asTracer, withSpan)
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.KeepAlive.Config (Config (..))
import Hoard.Types.Cardano (CardanoCodecs, CardanoMiniProtocol)


miniProtocol
    :: forall es
     . ( Clock :> es
       , Concurrent :> es
       , Pub KeepAlivePing :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Config
    -> CardanoCodecs
    -> Peer
    -> CardanoMiniProtocol
miniProtocol unlift conf codecs peer =
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
                                    withExceptionLogging "KeepAlive" $
                                        withSpan "keep_alive_protocol" $ do
                                            addEvent "protocol_started" []
                                            pure (keepAliveClientPeer $ client unlift conf peer)
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
       , Pub KeepAlivePing :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Config
    -> Peer
    -> KeepAliveClient IO ()
client unlift conf peer = KeepAliveClient sendFirst
  where
    sendFirst = unlift do
        addEvent "sending_first_keepalive" []
        timestamp <- Clock.currentTime
        publish KeepAlivePing {timestamp, peer}
        pure $ SendMsgKeepAlive (Cookie 42) sendNext

    sendNext = unlift do
        addEvent "keepalive_response_received" [("wait_seconds", show (conf.intervalMicroseconds `div` 1_000_000))]
        threadDelay conf.intervalMicroseconds
        addEvent "sending_keepalive" []
        pure $ SendMsgKeepAlive (Cookie 42) sendNext


data KeepAlivePing = KeepAlivePing
    { timestamp :: UTCTime
    , peer :: Peer
    }
    deriving (Typeable, Show)
