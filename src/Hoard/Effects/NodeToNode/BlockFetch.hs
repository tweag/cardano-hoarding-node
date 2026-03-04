module Hoard.Effects.NodeToNode.BlockFetch
    ( miniProtocol
    ) where

import Control.Tracer (nullTracer)
import Data.List (maximum, minimum)
import Effectful.Concurrent (Concurrent)
import Effectful.Timeout (Timeout)
import Network.Mux (StartOnDemandOrEagerly (..))
import Ouroboros.Consensus.Block.Abstract (headerPoint)
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
import Hoard.Data.BlockHash (BlockHash, blockHashFromHeader)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Chan (Chan, readChanBatched)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Metrics.Definitions (recordBlockFetchFailure)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, withSpan)
import Hoard.Effects.NodeToNode.Config (BlockFetchConfig (..))
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.Effects.UUID (GenUUID, UUID)
import Hoard.Events.BlockFetch
    ( BatchCompleted (..)
    , BlockReceived (..)
    , Request (..)
    , RequestFailed (..)
    , RequestStarted (..)
    )
import Hoard.Types.Cardano (CardanoBlock, CardanoCodecs, CardanoMiniProtocol, CardanoPoint)

import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Clock qualified as Clock
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
       , Pub BlockReceived :> es
       , Pub RequestFailed :> es
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
       , Pub BlockReceived :> es
       , Pub RequestFailed :> es
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
        timestamp <- Clock.currentTime
        publish $ RequestStarted {peer, timestamp}

        reqs <- do
            (inChan, outChan) <- Chan.newChan

            Conc.fork_ $ listen \(req :: Request) ->
                when (req.peer.id == peer.id) $ Chan.writeChan inChan req

            readChanBatched cfg.batchTimeoutMicroseconds cfg.batchSize outChan

        withSpan "block_fetch.start_fetch" do
            requestId <- UUID.gen
            let points = headerPoint . (.header) <$> reqs
                start = minimum points
                end = maximum points
                range = ChainRange start end
                requestedHashes = Set.fromList $ blockHashFromHeader . (.header) <$> toList reqs
            addAttribute "request.id" $ show @Text requestId
            addAttribute "request.count" $ length reqs

            pure
                $ BlockFetch.SendMsgRequestRange
                    range
                    (handleResponse unlift peer requestId requestedHashes reqs range)
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
    -> Set BlockHash
    -> ChainRange CardanoPoint
    -> Int
    -> BlockFetchReceiver CardanoBlock IO
blockReceiver unlift peer requestId requestedHashes range blockCount =
    BlockFetch.BlockFetchReceiver
        { handleBlock = \block -> unlift $ withSpan "block_fetch.handle_block" do
            addAttribute "request.id" $ show @Text requestId
            timestamp <- Clock.currentTime
            publish
                BlockReceived
                    { peer
                    , timestamp
                    , block
                    , requestId
                    , range
                    }
            pure $ blockReceiver unlift peer requestId requestedHashes range (blockCount + 1)
        , handleBatchDone = unlift $ withSpan "block_fetch.handle_batch_done" do
            addAttribute "request.id" $ show @Text requestId
            addAttribute "received.count" blockCount
            timestamp <- Clock.currentTime
            publish
                $ BatchCompleted
                    { peer
                    , timestamp
                    , blockCount
                    }
        }


handleResponse
    :: ( Clock :> es
       , Metrics :> es
       , Pub BatchCompleted :> es
       , Pub BlockReceived :> es
       , Pub RequestFailed :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Peer
    -> UUID
    -> Set BlockHash
    -> NonEmpty Request
    -> ChainRange CardanoPoint
    -> BlockFetchResponse CardanoBlock IO ()
handleResponse unlift peer requestId requestedHashes reqs range =
    BlockFetch.BlockFetchResponse
        { handleStartBatch = unlift $ withSpan "block_fetch.handle_start_batch" do
            addAttribute "request.id" $ show @Text requestId
            pure $ blockReceiver unlift peer requestId requestedHashes range 0
        , handleNoBlocks = unlift $ withSpan "block_fetch.handle_no_blocks" do
            addAttribute "request.id" $ show @Text requestId
            addAttribute "request.count" $ length reqs
            recordBlockFetchFailure
            timestamp <- Clock.currentTime
            for_ reqs \req ->
                publish
                    $ RequestFailed
                        { peer
                        , timestamp
                        , header = req.header
                        , errorMessage = "No blocks for point"
                        }
        }
