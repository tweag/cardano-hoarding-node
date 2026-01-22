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
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.KeepAlive.Config (Config (..))
import Hoard.Types.Cardano (CardanoCodecs, CardanoMiniProtocol)


miniProtocol
    :: forall es
     . ( Clock :> es
       , Concurrent :> es
       , Log :> es
       , Pub :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Config
    -> CardanoCodecs
    -> Peer
    -> CardanoMiniProtocol
miniProtocol unlift' conf codecs peer =
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
                                        pure (keepAliveClientPeer $ client unlift conf peer)
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
    :: ( Clock :> es
       , Concurrent :> es
       , Log :> es
       , Pub :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Config
    -> Peer
    -> KeepAliveClient IO ()
client unlift conf peer = KeepAliveClient sendFirst
  where
    sendFirst = unlift do
        Log.debug "Sending first keepalive message"
        timestamp <- Clock.currentTime
        publish KeepAlivePing {timestamp, peer}
        pure $ SendMsgKeepAlive (Cookie 42) sendNext

    sendNext = unlift do
        Log.debug $ "Response received, waiting " <> show (conf.intervalMicroseconds `div` 1_000_000) <> "s before next message"
        threadDelay conf.intervalMicroseconds
        Log.debug "Sending keepalive message"
        pure $ SendMsgKeepAlive (Cookie 42) sendNext


data KeepAlivePing = KeepAlivePing
    { timestamp :: UTCTime
    , peer :: Peer
    }
    deriving (Typeable, Show)
