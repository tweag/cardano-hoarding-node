module Hoard.Effects.NodeToNode.PeerSharing
    ( miniProtocol
    ) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Network.Mux (StartOnDemandOrEagerly (..))
import Network.Socket (SockAddr)
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , RunMiniProtocol (..)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.NodeToNode (peerSharingMiniProtocolNum)
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient (..), peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))
import Prelude hiding (Reader, State, ask, asks, evalState, runReader)

import Data.Set qualified as S
import Network.TypedProtocol.Peer.Client qualified as Peer

import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..), sockAddrToPeerAddress)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Monitoring.Tracing (Tracing, addEvent, asTracer, withSpan)
import Hoard.Effects.NodeToNode.Config (PeerSharingConfig (..))
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Events.PeerSharing (PeerSharingStarted (..), PeersReceived (..))
import Hoard.Types.Cardano (CardanoCodecs, CardanoMiniProtocol)

import Hoard.Effects.Clock qualified as Clock


miniProtocol
    :: forall es
     . ( Clock :> es
       , Concurrent :> es
       , Pub PeerSharingStarted :> es
       , Pub PeersReceived :> es
       , Tracing :> es
       )
    => PeerSharingConfig
    -> (forall x. Eff es x -> IO x)
    -> CardanoCodecs
    -> Peer
    -> CardanoMiniProtocol
miniProtocol conf unlift' codecs peer =
    MiniProtocol
        { miniProtocolNum = peerSharingMiniProtocolNum
        , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
        , miniProtocolStart = StartEagerly
        , miniProtocolRun =
            InitiatorProtocolOnly
                $ mkMiniProtocolCbFromPeer
                $ \_ ->
                    let peerSharingClient = client unlift conf peer
                        codec = cPeerSharingCodec codecs
                        wrappedPeer = Peer.Effect $ unlift $ withExceptionLogging "PeerSharing" $ withSpan "peer_sharing_protocol" $ do
                            timestamp <- Clock.currentTime
                            publish $ PeerSharingStarted {peer, timestamp}
                            addEvent "protocol_started" []
                            pure (peerSharingClientPeer peerSharingClient)
                        tracer = show >$< asTracer unlift "peer_sharing.protocol_message"
                    in  (tracer, codec, wrappedPeer)
        }
  where
    unlift :: forall x. Eff es x -> IO x
    unlift = unlift'


-- | Create a PeerSharing client that requests peer addresses.
--
-- This client:
-- 1. Requests peer addresses from the remote peer (amount configurable)
-- 2. Publishes a PeersReceived event with the results
-- 3. Waits for the configured interval
-- 4. Loops
client
    :: (Clock :> es, Concurrent :> es, Pub PeersReceived :> es, Tracing :> es)
    => (forall x. Eff es x -> IO x)
    -> PeerSharingConfig
    -> Peer
    -> PeerSharingClient SockAddr IO ()
client unlift conf peer = requestPeers withPeers
  where
    requestPeers = SendMsgShareRequest $ PeerSharingAmount $ fromIntegral conf.requestAmount
    withPeers peerAddrs = unlift do
        addEvent "peer_sharing_response_received" [("peer_count", show $ length peerAddrs)]
        timestamp <- Clock.currentTime
        publish
            $ PeersReceived
                { peer
                , timestamp
                , peerAddresses = S.fromList $ mapMaybe sockAddrToPeerAddress peerAddrs
                }
        addEvent "waiting_for_next_request" [("wait_seconds", show (conf.requestIntervalMicroseconds `div` 1_000_000))]
        threadDelay conf.requestIntervalMicroseconds
        addEvent "sending_next_request" []
        pure $ requestPeers withPeers
