module Hoard.Effects.NodeToNode.PeerSharing
    ( miniProtocol
    ) where

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

import Data.Set qualified as S
import Network.TypedProtocol.Peer.Client qualified as Peer

import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..), sockAddrToPeerAddress)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Tracing (Tracing, asTracer)
import Hoard.Effects.NodeToNode.Config (PeerSharingConfig (..))
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Events.PeerSharing (PeersReceived (..))
import Hoard.Types.Cardano (CardanoCodecs, CardanoMiniProtocol)

import Hoard.Effects.Clock qualified as Clock


miniProtocol
    :: forall es
     . ( Clock :> es
       , Concurrent :> es
       , Log :> es
       , Pub PeersReceived :> es
       , Tracing :> es
       )
    => PeerSharingConfig
    -> (forall x. Eff es x -> IO x)
    -> CardanoCodecs
    -> Peer
    -> CardanoMiniProtocol
miniProtocol conf unlift codecs peer =
    MiniProtocol
        { miniProtocolNum = peerSharingMiniProtocolNum
        , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
        , miniProtocolStart = StartEagerly
        , miniProtocolRun =
            InitiatorProtocolOnly
                $ mkMiniProtocolCbFromPeer
                $ \_ ->
                    let codec = cPeerSharingCodec codecs
                        wrappedPeer =
                            Peer.Effect
                                $ unlift
                                $ withExceptionLogging "PeerSharing"
                                $ pure
                                $ peerSharingClientPeer
                                $ client unlift conf peer
                        tracer = show >$< asTracer unlift "peer_sharing.protocol_message"
                    in  (tracer, codec, wrappedPeer)
        }


-- | Create a PeerSharing client that requests peer addresses.
--
-- This client:
-- 1. Requests peer addresses from the remote peer (amount configurable)
-- 2. Publishes a PeersReceived event with the results
-- 3. Waits for the configured interval
-- 4. Loops
client
    :: (Clock :> es, Concurrent :> es, Pub PeersReceived :> es)
    => (forall x. Eff es x -> IO x)
    -> PeerSharingConfig
    -> Peer
    -> PeerSharingClient SockAddr IO ()
client unlift conf peer = requestPeers withPeers
  where
    requestPeers = SendMsgShareRequest $ PeerSharingAmount $ fromIntegral conf.requestAmount
    withPeers peerAddrs = unlift do
        timestamp <- Clock.currentTime
        publish
            $ PeersReceived
                { peer
                , timestamp
                , peerAddresses = S.fromList $ mapMaybe sockAddrToPeerAddress peerAddrs
                }
        threadDelay conf.requestIntervalMicroseconds
        pure $ requestPeers withPeers
