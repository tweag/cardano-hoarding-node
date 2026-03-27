module Hoard.Effects.NodeToNode.TxSubmission
    ( miniProtocol
    ) where

import Control.Tracer (nullTracer)
import Network.Mux (MiniProtocolLimits (..), MiniProtocolNum (..), StartOnDemandOrEagerly (..))
import Network.TypedProtocol.Core (N (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId)
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolCb (..)
    , RunMiniProtocol (..)
    , mkMiniProtocolCbFromPeerPipelined
    )
import Ouroboros.Network.Protocol.TxSubmission2.Server
    ( Collect (..)
    , ServerStIdle (..)
    , TxSubmissionServerPipelined (..)
    , txSubmissionServerPeerPipelined
    )
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..), NumTxIdsToReq (..))

import Atelier.Effects.Log (Log)
import Atelier.Effects.Publishing (Pub, publish)
import Hoard.Data.Peer (Peer (..))
import Hoard.Events.TxSubmission (TxReceived (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoCodecs, CardanoMiniProtocol)

import Atelier.Effects.Log qualified as Log


miniProtocol
    :: forall es
     . ( Log :> es
       , Pub TxReceived :> es
       )
    => (forall x. Eff es x -> IO x)
    -> CardanoCodecs
    -> Peer
    -> CardanoMiniProtocol
miniProtocol unlift codecs peer =
    MiniProtocol
        { miniProtocolNum = MiniProtocolNum 4
        , miniProtocolLimits = MiniProtocolLimits maxBound
        , miniProtocolStart = StartOnDemand
        , miniProtocolRun =
            InitiatorAndResponderProtocol
                (MiniProtocolCb $ \_ _ -> pure ((), Nothing))
                ( mkMiniProtocolCbFromPeerPipelined $ \_ ->
                    ( nullTracer
                    , cTxSubmission2Codec codecs
                    , txSubmissionServerPeerPipelined (receiveLoop unlift peer)
                    )
                )
        }


receiveLoop
    :: ( Log :> es
       , Pub TxReceived :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Peer
    -> TxSubmissionServerPipelined (GenTxId CardanoBlock) (GenTx CardanoBlock) IO ()
receiveLoop unlift peer = TxSubmissionServerPipelined $ unlift do
    Log.debug $ "tx_submission: server started for peer=" <> show peer.address
    pure $ idle (NumTxIdsToAck 0)
  where
    idle :: NumTxIdsToAck -> ServerStIdle Z (GenTxId CardanoBlock) (GenTx CardanoBlock) IO ()
    idle ackN =
        SendMsgRequestTxIdsBlocking
            ackN
            (NumTxIdsToReq txBatchSize)
            (unlift $ Log.debug $ "tx_submission: session closed by peer=" <> show peer.address)
            ( \txids -> unlift do
                Log.debug $ "tx_submission: " <> show (length txids) <> " txids advertised by peer=" <> show peer.address
                pure $ fetch (NumTxIdsToAck (fromIntegral (length txids))) (map fst (Prelude.toList txids))
            )

    fetch :: NumTxIdsToAck -> [GenTxId CardanoBlock] -> ServerStIdle Z (GenTxId CardanoBlock) (GenTx CardanoBlock) IO ()
    fetch ackN txids
        | null txids = idle ackN
        | otherwise =
            SendMsgRequestTxsPipelined txids
                $ pure
                $ CollectPipelined Nothing
                $ \case
                    CollectTxs _ txs -> unlift do
                        Log.debug $ "tx_submission: " <> show (length txs) <> " txs received from peer=" <> show peer.address
                        for_ txs $ \tx -> do
                            Log.debug $ "tx_submission: new tx from peer=" <> show peer.address
                            publish TxReceived {peer, tx}
                        pure $ idle ackN
                    CollectTxIds _ _ -> pure $ idle ackN

    txBatchSize :: Word16
    txBatchSize = 10
