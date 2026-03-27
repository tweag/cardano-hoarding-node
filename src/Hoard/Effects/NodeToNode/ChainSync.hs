module Hoard.Effects.NodeToNode.ChainSync
    ( miniProtocol
    ) where

import Cardano.Api.Block (toConsensusPointHF)
import Control.Tracer (nullTracer)
import Effectful.State.Static.Shared (State, gets)
import Network.Mux (StartOnDemandOrEagerly (..))
import Network.TypedProtocol (PeerRole (..))
import Network.TypedProtocol.Peer.Client
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolCb (..)
    , MiniProtocolLimits (..)
    , RunMiniProtocol (..)
    , mkMiniProtocolCbFromPeerPipelined
    )
import Ouroboros.Network.NodeToNode (chainSyncMiniProtocolNum)
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

import Data.List qualified as List
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync

import Atelier.Effects.Log (Log)
import Atelier.Effects.Monitoring.Metrics (Metrics)
import Atelier.Effects.Monitoring.Tracing (Tracing, withSpan)
import Atelier.Effects.Publishing (Pub, publish)
import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Monitoring.Metrics.Definitions (recordChainSyncRollback, recordChainSyncRollforward)
import Hoard.Effects.NodeToNode.Config (ChainSyncConfig (..))
import Hoard.Events.ChainSync (HeaderReceived (..), IntersectionFound (..), RollBackward (..))
import Hoard.Types.Cardano (CardanoCodecs, CardanoHeader, CardanoMiniProtocol, CardanoPoint, CardanoTip, ChainPoint (ChainPoint))
import Hoard.Types.HoardState (HoardState (..))


-- | ChainSync mini-protocol (pipelined)
miniProtocol
    :: forall es
     . ( Log :> es
       , Metrics :> es
       , Pub HeaderReceived :> es
       , Pub IntersectionFound :> es
       , Pub RollBackward :> es
       , State HoardState :> es
       , Tracing :> es
       )
    => ChainSyncConfig
    -> (forall x. Eff es x -> IO x)
    -> CardanoCodecs
    -> Peer
    -> CardanoMiniProtocol
miniProtocol conf unlift codecs peer =
    MiniProtocol
        { miniProtocolNum = chainSyncMiniProtocolNum
        , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
        , miniProtocolStart = StartEagerly
        , miniProtocolRun =
            InitiatorAndResponderProtocol
                ( mkMiniProtocolCbFromPeerPipelined $ \_ ->
                    let codec = cChainSyncCodec codecs
                        chainSyncClient = client unlift peer
                    in  (nullTracer, codec, chainSyncClient)
                )
                (MiniProtocolCb $ \_ _ -> pure ((), Nothing))
        }


-- | Create a ChainSync client that synchronizes chain headers (pipelined version).
--
-- This client:
-- 1. Finds an intersection starting from genesis
-- 2. Requests headers continuously
-- 3. Publishes HeaderReceived events for each header
-- 4. Handles rollbacks by publishing RollBackward events
--
-- Note: This runs forever, continuously requesting the next header.
client
    :: forall es
     . ( Log :> es
       , Metrics :> es
       , Pub HeaderReceived :> es
       , Pub IntersectionFound :> es
       , Pub RollBackward :> es
       , State HoardState :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Peer
    -> PeerPipelined (ChainSync CardanoHeader CardanoPoint CardanoTip) AsClient ChainSync.StIdle IO ()
client unlift peer =
    ClientPipelined $ Effect $ unlift $ withExceptionLogging "ChainSync" do
        initialPoints <-
            fmap List.singleton
                . fmap toConsensusPointHF
                . coerce
                . gets @HoardState
                $ (.immutableTip)
        pure (findIntersect initialPoints)
  where
    findIntersect :: forall c. [CardanoPoint] -> Client (ChainSync CardanoHeader CardanoPoint CardanoTip) (Pipelined Z c) ChainSync.StIdle IO ()
    findIntersect initialPoints =
        Yield (ChainSync.MsgFindIntersect initialPoints) $ Await $ \case
            ChainSync.MsgIntersectNotFound _ ->
                Effect
                    $ unlift
                    $ withSpan "chain_sync.intersection_not_found"
                    $ pure requestNext
            ChainSync.MsgIntersectFound point tip -> Effect $ unlift $ withSpan "chain_sync.intersection_found" do
                publish
                    $ IntersectionFound
                        { peer
                        , point
                        , tip
                        }
                pure requestNext

    requestNext :: forall c. Client (ChainSync CardanoHeader CardanoPoint CardanoTip) (Pipelined Z c) ChainSync.StIdle IO ()
    requestNext =
        Yield ChainSync.MsgRequestNext $ Await $ \case
            ChainSync.MsgRollForward header tip -> Effect $ unlift $ withSpan "chain_sync.rollforward" do
                recordChainSyncRollforward
                let event =
                        HeaderReceived
                            { peer
                            , header
                            , tip
                            }
                publish event
                pure requestNext
            ChainSync.MsgRollBackward point tip -> Effect $ unlift $ withSpan "chain_sync.rollback" do
                recordChainSyncRollback
                publish
                    $ RollBackward
                        { peer
                        , point
                        , tip
                        }
                pure requestNext
            ChainSync.MsgAwaitReply -> Await $ \case
                ChainSync.MsgRollForward header tip -> Effect $ unlift $ withSpan "chain_sync.rollforward_after_await" do
                    recordChainSyncRollforward
                    publish
                        $ HeaderReceived
                            { peer
                            , header
                            , tip
                            }
                    pure requestNext
                ChainSync.MsgRollBackward point tip -> Effect $ unlift $ withSpan "chain_sync.rollback_after_await" do
                    recordChainSyncRollback
                    publish
                        $ RollBackward
                            { peer
                            , point
                            , tip
                            }
                    pure requestNext
