module Hoard.Effects.NodeToNode.BlockFetch
    ( miniProtocol
    ) where

import Control.Tracer (nullTracer)
import Data.List (maximum, minimum)
import Effectful.Concurrent (Concurrent)
import Effectful.Timeout (Timeout)
import Network.Mux (StartOnDemandOrEagerly (..))
import Ouroboros.Consensus.Block.Abstract (blockSlot, headerPoint)
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , RunMiniProtocol (..)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.NodeToNode (blockFetchMiniProtocolNum)
import Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchReceiver, BlockFetchResponse, blockFetchClientPeer)
import Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))

import Data.Set qualified as Set
import Network.TypedProtocol.Peer.Client qualified as Peer
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BlockFetch

import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.BlockHash (BlockHash, mkBlockHash)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Chan (Chan, readChanBatched)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Metrics.Definitions (recordBlockFetchFailure)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, withSpan)
import Hoard.Effects.NodeToNode.Config (BlockFetchConfig (..))
import Hoard.Effects.Publishing (Pub, Sub, listen_, publish)
import Hoard.Effects.UUID (GenUUID, UUID)
import Hoard.Events.BlockFetch
    ( BatchCompleted (..)
    , BatchFailed (..)
    , BlockReceived (..)
    , Request (..)
    , RequestStarted (..)
    )
import Hoard.Types.Cardano (CardanoBlock, CardanoCodecs, CardanoHeader, CardanoMiniProtocol, CardanoPoint)

import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.UUID qualified as UUID


miniProtocol
    :: forall es
     . ( Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , GenUUID :> es
       , Log :> es
       , Metrics :> es
       , Pub BatchCompleted :> es
       , Pub BatchFailed :> es
       , Pub BlockReceived :> es
       , Pub RequestStarted :> es
       , Sub Request :> es
       , Timeout :> es
       , Tracing :> es
       )
    => BlockFetchConfig
    -> (forall x. Eff es x -> IO x)
    -> CardanoCodecs
    -> Peer
    -> CardanoMiniProtocol
miniProtocol conf unlift codecs peer =
    MiniProtocol
        { miniProtocolNum = blockFetchMiniProtocolNum
        , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
        , miniProtocolStart = StartEagerly
        , miniProtocolRun = InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer $ \_ ->
            let codec = cBlockFetchCodec codecs
                wrappedPeer =
                    Peer.Effect
                        $ unlift
                        $ withExceptionLogging "BlockFetch"
                        $ pure
                        $ blockFetchClientPeer
                        $ client unlift conf peer
            in  (nullTracer, codec, wrappedPeer)
        }


-- | Create a BlockFetch client that fetches blocks on requests over events.
client
    :: forall es
     . ( Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , GenUUID :> es
       , Metrics :> es
       , Pub BatchCompleted :> es
       , Pub BatchFailed :> es
       , Pub BlockReceived :> es
       , Pub RequestStarted :> es
       , Sub Request :> es
       , Timeout :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> BlockFetchConfig
    -> Peer
    -> BlockFetch.BlockFetchClient CardanoBlock CardanoPoint IO ()
client unlift cfg peer =
    BlockFetch.BlockFetchClient $ unlift do
        publish $ RequestStarted peer

        reqs <- do
            (inChan, outChan) <- Chan.newChan

            Conc.fork_ $ listen_ \(req :: Request) ->
                when (req.peer.id == peer.id) $ Chan.writeChan inChan req

            readChanBatched cfg.batchTimeoutMicroseconds cfg.batchSize outChan

        withSpan "block_fetch.start_fetch" do
            requestId <- UUID.gen
            let headers = (.header) <$> reqs
                points = headerPoint <$> headers
                start = minimum points
                end = maximum points
                range = ChainRange start end
                requestedHashes = Set.fromList $ mkBlockHash <$> toList headers
            addAttribute "request.id" $ show @Text requestId
            addAttribute "request.count" $ length headers

            pure
                $ BlockFetch.SendMsgRequestRange
                    range
                    (handleResponse unlift peer requestId requestedHashes headers range)
                $ client unlift cfg peer


blockReceiver
    :: ( Clock :> es
       , Pub BatchCompleted :> es
       , Pub BlockReceived :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Peer
    -> UUID
    -> NonEmpty CardanoHeader
    -> Set BlockHash
    -> ChainRange CardanoPoint
    -> Int
    -> BlockFetchReceiver CardanoBlock IO
blockReceiver unlift peer requestId reqs requestedHashes range blockCount =
    BlockFetch.BlockFetchReceiver
        { handleBlock = \block -> unlift $ withSpan "block_fetch.handle_block" do
            addAttribute "request.id" $ show @Text requestId
            let slotNumber = blockSlot block
                headerWithSameSlotNumber = find ((== slotNumber) . blockSlot) reqs
            publish
                BlockReceived
                    { peer
                    , block
                    , requestId
                    , headerWithSameSlotNumber
                    , range
                    }
            pure $ blockReceiver unlift peer requestId reqs requestedHashes range (blockCount + 1)
        , handleBatchDone = unlift $ withSpan "block_fetch.handle_batch_done" do
            addAttribute "request.id" $ show @Text requestId
            addAttribute "received.count" blockCount
            publish
                $ BatchCompleted
                    { peer
                    , blockCount
                    }
        }


handleResponse
    :: ( Clock :> es
       , Metrics :> es
       , Pub BatchCompleted :> es
       , Pub BatchFailed :> es
       , Pub BlockReceived :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Peer
    -> UUID
    -> Set BlockHash
    -> NonEmpty CardanoHeader
    -> ChainRange CardanoPoint
    -> BlockFetchResponse CardanoBlock IO ()
handleResponse unlift peer requestId requestedHashes headers range =
    BlockFetch.BlockFetchResponse
        { handleStartBatch = unlift $ withSpan "block_fetch.handle_start_batch" do
            addAttribute "request.id" $ show @Text requestId
            pure $ blockReceiver unlift peer requestId headers requestedHashes range 0
        , handleNoBlocks = unlift $ withSpan "block_fetch.handle_no_blocks" do
            addAttribute "request.id" $ show @Text requestId
            addAttribute "request.count" $ length headers
            recordBlockFetchFailure
            publish
                BatchFailed
                    { peer
                    , headers
                    , errorMessage = "No blocks for points"
                    }
        }
