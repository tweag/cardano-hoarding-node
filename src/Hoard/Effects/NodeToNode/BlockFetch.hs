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
import Ouroboros.Network.Protocol.BlockFetch.Client (blockFetchClientPeer)
import Prelude hiding (Reader, State, ask, evalState, get, modify, runReader)

import Data.Set qualified as Set
import Network.TypedProtocol.Peer.Client qualified as Peer
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch

import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.BlockHash (blockHashFromHeader)
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
                wrappedPeer = Peer.Effect $ unlift $ withExceptionLogging "BlockFetch" $ withSpan "block_fetch_protocol" do
                    addEvent "protocol_started" []
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
    BlockFetch.BlockFetchClient $ unlift do
        timestamp <- Clock.currentTime
        publish $ RequestStarted {peer, timestamp}
        addEvent "awaiting_block_requests" []

        (inChan, outChan) <- Chan.newChan

        Conc.fork_ $ listen \(req :: Request) ->
            when (req.peer.id == peer.id) $ Chan.writeChan inChan req

        awaitMessage outChan
  where
    awaitMessage outChan = do
        reqs <- readChanBatched cfg.batchTimeoutMicroseconds cfg.batchSize outChan

        addEvent "block_fetch_requests_received" [("count", show $ length reqs)]
        requestId <- UUID.gen
        let points = headerPoint . (.header) <$> reqs
            start = minimum points
            end = maximum points
            requestedHashes = Set.fromList $ blockHashFromHeader . (.header) <$> toList reqs
        addAttribute "range.start" (show start)
        addAttribute "range.end" (show end)
        addEvent
            "block_fetch_requests_received"
            [ ("request.id", show requestId)
            , ("range.count", show $ length reqs)
            , ("range.start", show start)
            , ("range.end", show end)
            ]
        pure
            $ BlockFetch.SendMsgRequestRange
                (BlockFetch.ChainRange start end)
                (handleResponse requestId requestedHashes reqs)
            $ client unlift cfg peer

    handleResponse requestId requestedHashes reqs =
        BlockFetch.BlockFetchResponse
            { handleStartBatch = do
                unlift $ addEvent "block_fetch_request_start" [("request.id", show requestId)]
                pure $ blockReceiver requestId requestedHashes 0
            , handleNoBlocks = unlift $ do
                recordBlockFetchFailure
                addEvent
                    "no_blocks_returned"
                    [ ("request.id", show requestId)
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

    blockReceiver requestId requestedHashes blockCount =
        BlockFetch.BlockFetchReceiver
            { handleBlock = \block -> unlift $ do
                timestamp <- Clock.currentTime
                let blockHash = blockHashFromHeader $ getHeader block
                if (blockHash `Set.member` requestedHashes) then
                    publish
                        BlockReceived
                            { peer
                            , timestamp
                            , block
                            , requestId
                            }
                else
                    addEvent
                        "block_fetch_received_unrequested_block"
                        [ ("request.id", show requestId)
                        , ("block.hash", show (blockHashFromHeader $ getHeader block))
                        ]
                pure $ blockReceiver requestId requestedHashes $ blockCount + 1
            , handleBatchDone = unlift $ do
                addEvent "batch_completed" [("received.count", show blockCount)]
                timestamp <- Clock.currentTime
                publish
                    $ BatchCompleted
                        { peer
                        , timestamp
                        , blockCount
                        }
            }
