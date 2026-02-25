module Hoard.Effects.NodeToNode.ChainSync
    ( miniProtocol
    ) where

import Cardano.Api.Block (toConsensusPointHF)
import Control.Tracer (nullTracer)
import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State, gets)
import Network.Mux (StartOnDemandOrEagerly (..))
import Network.TypedProtocol (PeerRole (..))
import Network.TypedProtocol.Peer.Client
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , RunMiniProtocol (..)
    , mkMiniProtocolCbFromPeerPipelined
    )
import Ouroboros.Network.NodeToNode (chainSyncMiniProtocolNum)
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Prelude hiding (Reader, State, ask, gets, runReader)

import Data.List qualified as List
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync

import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Metrics.Definitions (recordChainSyncRollback, recordChainSyncRollforward)
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Effects.NodeToNode.Config (ChainSyncConfig (..))
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Events.ChainSync
    ( ChainSyncIntersectionFound (..)
    , ChainSyncStarted (..)
    , HeaderReceived (..)
    , RollBackward (..)
    )
import Hoard.Types.Cardano (CardanoCodecs, CardanoHeader, CardanoMiniProtocol, CardanoPoint, CardanoTip, ChainPoint (ChainPoint))
import Hoard.Types.HoardState (HoardState (..))

import Hoard.Effects.Clock qualified as Clock


-- | ChainSync mini-protocol (pipelined)
miniProtocol
    :: forall es
     . ( Clock :> es
       , Log :> es
       , Metrics :> es
       , Pub ChainSyncIntersectionFound :> es
       , Pub ChainSyncStarted :> es
       , Pub HeaderReceived :> es
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
            InitiatorProtocolOnly
                $ mkMiniProtocolCbFromPeerPipelined
                $ \_ ->
                    let codec = cChainSyncCodec codecs
                        chainSyncClient = client unlift peer
                    in  (nullTracer, codec, chainSyncClient)
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
     . ( Clock :> es
       , Log :> es
       , Metrics :> es
       , Pub ChainSyncIntersectionFound :> es
       , Pub ChainSyncStarted :> es
       , Pub HeaderReceived :> es
       , Pub RollBackward :> es
       , State HoardState :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Peer
    -> PeerPipelined (ChainSync CardanoHeader CardanoPoint CardanoTip) AsClient ChainSync.StIdle IO ()
client unlift peer =
    ClientPipelined
        $ Effect
        $ unlift
        $ withExceptionLogging "ChainSync"
        $ do
            timestamp <- Clock.currentTime
            publish $ ChainSyncStarted {peer, timestamp}
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
                timestamp <- Clock.currentTime
                publish
                    $ ChainSyncIntersectionFound
                        { peer
                        , timestamp
                        , point
                        , tip
                        }
                pure requestNext

    requestNext :: forall c. Client (ChainSync CardanoHeader CardanoPoint CardanoTip) (Pipelined Z c) ChainSync.StIdle IO ()
    requestNext =
        Yield ChainSync.MsgRequestNext $ Await $ \case
            ChainSync.MsgRollForward header tip -> Effect $ unlift $ withSpan "chain_sync.rollforward" do
                recordChainSyncRollforward
                timestamp <- Clock.currentTime
                let event =
                        HeaderReceived
                            { peer
                            , timestamp
                            , header
                            , tip
                            }
                publish event
                pure requestNext
            ChainSync.MsgRollBackward point tip -> Effect $ unlift $ withSpan "chain_sync.rollback" do
                recordChainSyncRollback
                timestamp <- Clock.currentTime
                publish
                    $ RollBackward
                        { peer
                        , timestamp
                        , point
                        , tip
                        }
                pure requestNext
            ChainSync.MsgAwaitReply -> Await $ \case
                ChainSync.MsgRollForward header tip -> Effect $ unlift $ withSpan "chain_sync.rollforward_after_await" do
                    recordChainSyncRollforward
                    timestamp <- Clock.currentTime
                    publish
                        $ HeaderReceived
                            { peer
                            , timestamp
                            , header
                            , tip
                            }
                    pure requestNext
                ChainSync.MsgRollBackward point tip -> Effect $ unlift $ withSpan "chain_sync.rollback_after_await" do
                    recordChainSyncRollback
                    timestamp <- Clock.currentTime
                    publish
                        $ RollBackward
                            { peer
                            , timestamp
                            , point
                            , tip
                            }
                    pure requestNext
