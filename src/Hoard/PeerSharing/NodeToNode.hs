module Hoard.PeerSharing.NodeToNode (miniProtocol, client) where

import Data.Set qualified as S
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Network.Mux (StartOnDemandOrEagerly (..))
import Network.Socket (SockAddr)
import Network.TypedProtocol.Peer.Client qualified as Peer
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , RunMiniProtocol (..)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.NodeToNode
    ( peerSharingMiniProtocolNum
    )
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient (..), peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))
import Prelude hiding (Reader, State, asks, evalState)

import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..), sockAddrToPeerAddress)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Monitoring.Tracing (Tracing, addEvent, asTracer, withSpan)
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.PeerSharing.Config (Config (..))
import Hoard.PeerSharing.Events (PeerSharingStarted (..), PeersReceived (..))
import Hoard.Types.Cardano (CardanoCodecs, CardanoMiniProtocol)


miniProtocol
    :: forall es
     . ( Clock :> es
       , Concurrent :> es
       , Pub :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Config
    -> CardanoCodecs
    -> Peer
    -> CardanoMiniProtocol
miniProtocol unlift' conf codecs peer =
    MiniProtocol
        { miniProtocolNum = peerSharingMiniProtocolNum
        , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
        , miniProtocolStart = StartEagerly
        , miniProtocolRun =
            InitiatorProtocolOnly $
                mkMiniProtocolCbFromPeer $
                    \_ ->
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


--------------------------------------------------------------------------------
-- PeerSharing Protocol Implementation
--------------------------------------------------------------------------------

-- | Create a PeerSharing client that requests peer addresses.
--
-- This client:
-- 1. Requests peer addresses from the remote peer (amount configurable)
-- 2. Publishes a PeersReceived event with the results
-- 3. Waits for the configured interval
-- 4. Loops
client
    :: (Concurrent :> es, Clock :> es, Tracing :> es, Pub :> es)
    => (forall x. Eff es x -> IO x)
    -> Config
    -> Peer
    -> PeerSharingClient SockAddr IO ()
client unlift conf peer = requestPeers withPeers
  where
    requestPeers = SendMsgShareRequest $ PeerSharingAmount $ fromIntegral conf.requestAmount
    withPeers peerAddrs = unlift do
        addEvent "peer_sharing_response_received" [("peer_count", show $ length peerAddrs)]
        timestamp <- Clock.currentTime
        publish $
            PeersReceived
                { peer
                , timestamp
                , peerAddresses = S.fromList $ mapMaybe sockAddrToPeerAddress peerAddrs
                }
        addEvent "waiting_for_next_request" [("wait_seconds", show (conf.requestIntervalMicroseconds `div` 1_000_000))]
        threadDelay conf.requestIntervalMicroseconds
        addEvent "sending_next_request" []
        pure $ requestPeers withPeers
