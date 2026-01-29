module Hoard.ChainSync.NodeToNode (miniProtocol, client) where

import Cardano.Api.Block (toConsensusPointHF)
import Control.Tracer (nullTracer)
import Data.List qualified as List
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
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Prelude hiding (State, gets)

import Hoard.ChainSync.Config (Config (..))
import Hoard.ChainSync.Events
    ( ChainSyncIntersectionFound (..)
    , ChainSyncStarted (..)
    , HeaderReceived (..)
    , RollBackward (..)
    )
import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Types.Cardano (CardanoCodecs, CardanoHeader, CardanoMiniProtocol, CardanoPoint, CardanoTip, ChainPoint (ChainPoint))
import Hoard.Types.HoardState (HoardState (..))


-- | ChainSync mini-protocol (pipelined)
miniProtocol
    :: forall es
     . ( State HoardState :> es
       , Clock :> es
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
        { miniProtocolNum = chainSyncMiniProtocolNum
        , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
        , miniProtocolStart = StartEagerly
        , miniProtocolRun =
            InitiatorProtocolOnly $
                mkMiniProtocolCbFromPeerPipelined $
                    \_ ->
                        let codec = cChainSyncCodec codecs
                            -- Note: Exception logging added inside chainSyncClientImpl via Effect
                            chainSyncClient = client unlift peer
                        in  (nullTracer, codec, chainSyncClient)
        }
  where
    unlift :: forall x. Eff es x -> IO x
    unlift = unlift' . Log.withNamespace "ChainSync"


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
       , Pub :> es
       , State HoardState :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Peer
    -> PeerPipelined (ChainSync CardanoHeader CardanoPoint CardanoTip) AsClient ChainSync.StIdle IO ()
client unlift peer =
    ClientPipelined $
        Effect $
            unlift $
                withExceptionLogging "ChainSync" $
                    do
                        -- Publish started event
                        timestamp <- Clock.currentTime
                        publish $ ChainSyncStarted {peer, timestamp}
                        Log.debug "Published ChainSyncStarted event"
                        Log.debug "Starting pipelined client, finding intersection from genesis"
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
            ChainSync.MsgIntersectNotFound {} -> Effect $ unlift $ do
                Log.debug "Intersection not found (continuing anyway)"
                pure requestNext
            ChainSync.MsgIntersectFound point tip -> Effect $ unlift $ do
                Log.debug "Intersection found"
                timestamp <- Clock.currentTime
                publish $
                    ChainSyncIntersectionFound
                        { peer
                        , timestamp
                        , point
                        , tip
                        }
                pure requestNext

    requestNext :: forall c. Client (ChainSync CardanoHeader CardanoPoint CardanoTip) (Pipelined Z c) ChainSync.StIdle IO ()
    requestNext =
        Yield ChainSync.MsgRequestNext $ Await $ \case
            ChainSync.MsgRollForward header tip -> Effect $ unlift $ do
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
            ChainSync.MsgRollBackward point tip -> Effect $ unlift $ do
                Log.debug "Rollback"
                timestamp <- Clock.currentTime
                publish $
                    RollBackward
                        { peer
                        , timestamp
                        , point
                        , tip
                        }
                pure requestNext
            ChainSync.MsgAwaitReply -> Await $ \case
                ChainSync.MsgRollForward header tip -> Effect $ unlift $ do
                    timestamp <- Clock.currentTime
                    publish $
                        HeaderReceived
                            { peer
                            , timestamp
                            , header
                            , tip
                            }
                    pure requestNext
                ChainSync.MsgRollBackward point tip -> Effect $ unlift $ do
                    Log.debug "Rollback after await"
                    timestamp <- Clock.currentTime
                    publish $
                        RollBackward
                            { peer
                            , timestamp
                            , point
                            , tip
                            }
                    pure requestNext
