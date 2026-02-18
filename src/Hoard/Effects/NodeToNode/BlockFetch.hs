module Hoard.Effects.NodeToNode.BlockFetch
    ( miniProtocol
    ) where

import Control.Tracer (nullTracer)
import Data.List (maximum, minimum)
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Timeout (Timeout)
import Network.Mux (StartOnDemandOrEagerly (..))
import Ouroboros.Consensus.Block.Abstract (getHeader, headerPoint)
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , RunMiniProtocol (..)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.NodeToNode (blockFetchMiniProtocolNum)
import Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchReceiver, BlockFetchResponse, blockFetchClientPeer)
import Prelude hiding (Reader, State, ask, evalState, get, modify, runReader)

import Data.Set qualified as Set
import Network.TypedProtocol.Peer.Client qualified as Peer
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch

import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.BlockHash (BlockHash, blockHashFromHeader)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Chan (Chan, readChanBatched)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Metrics.Definitions (recordBlockFetchFailure)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, addEvent, withSpan)
import Hoard.Effects.NodeToNode.Config (BlockFetchConfig (..))
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.Effects.UUID (GenUUID)
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
                blockFetchClient = client unlift conf peer
                tracer = nullTracer
                wrappedPeer =
                    Peer.Effect
                        $ unlift
                        $ withExceptionLogging "BlockFetch"
                        $ withSpan "block_fetch_protocol"
                        $ do
                            addAttribute "peer.id" peer.id
                            addAttribute "peer.address" peer.address
                            pure $ blockFetchClientPeer blockFetchClient
            in  (tracer, codec, wrappedPeer)
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
    BlockFetch.BlockFetchClient $ unlift $ withSpan "block_fetch.client" do
        timestamp <- Clock.currentTime
        publish $ RequestStarted {peer, timestamp}

        reqs <- withSpan "block_fetch.await" do
            (inChan, outChan) <- Chan.newChan

            Conc.fork_ $ listen \(req :: Request) ->
                when (req.peer.id == peer.id) $ Chan.writeChan inChan req

            readChanBatched cfg.batchTimeoutMicroseconds cfg.batchSize outChan

        withSpan "block_fetch.start_fetch" do
            requestId <- show <$> UUID.gen
            let points = headerPoint . (.header) <$> reqs
                start = minimum points
                end = maximum points
                requestedHashes = Set.fromList $ blockHashFromHeader . (.header) <$> toList reqs
            addAttribute "request.id" requestId
            addAttribute "request.count" $ length reqs
            addAttribute "range.start" (show @Text start)
            addAttribute "range.end" (show @Text end)

            pure
                $ BlockFetch.SendMsgRequestRange
                    (BlockFetch.ChainRange start end)
                    (handleResponse unlift peer requestId requestedHashes reqs)
                $ client unlift cfg peer


blockReceiver
    :: ( Clock :> es
       , Pub BatchCompleted :> es
       , Pub BlockReceived :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Peer
    -> Text
    -> Set BlockHash
    -> Int
    -> BlockFetchReceiver CardanoBlock IO
blockReceiver unlift peer requestId requestedHashes blockCount =
    BlockFetch.BlockFetchReceiver
        { handleBlock = \block -> unlift $ withSpan "block_fetch.handle_block" do
            addAttribute "request.id" requestId
            addAttribute "block.hash" $ blockHashFromHeader $ getHeader block
            timestamp <- Clock.currentTime
            publish
                BlockReceived
                    { peer
                    , timestamp
                    , block
                    }
            pure $ blockReceiver unlift peer requestId requestedHashes (blockCount + 1)
        , handleBatchDone = unlift $ withSpan "block_fetch.handle_batch_done" do
            addAttribute "request.id" requestId
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
    -> Text
    -> Set BlockHash
    -> NonEmpty Request
    -> BlockFetchResponse CardanoBlock IO ()
handleResponse unlift peer requestId requestedHashes reqs =
    BlockFetch.BlockFetchResponse
        { handleStartBatch = do
            unlift $ addEvent "block_fetch_request_start" [("request.id", requestId)]
            pure $ blockReceiver unlift peer requestId requestedHashes 0
        , handleNoBlocks = unlift $ do
            recordBlockFetchFailure
            addEvent
                "no_blocks_returned"
                [ ("request.id", requestId)
                , ("request.count", show $ length reqs)
                ]
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
