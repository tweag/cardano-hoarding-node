module Hoard.BlockFetch.NodeToNode (miniProtocol, client) where

import Control.Tracer (nullTracer)
import Data.List (maximum, minimum)
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.State.Static.Shared (State)
import Effectful.Timeout (Timeout)
import Network.Mux (StartOnDemandOrEagerly (..))
import Network.TypedProtocol.Peer.Client qualified as Peer
import Ouroboros.Consensus.Block.Abstract (headerPoint)
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , RunMiniProtocol (..)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.NodeToNode
    ( blockFetchMiniProtocolNum
    )
import Ouroboros.Network.Protocol.BlockFetch.Client (blockFetchClientPeer)
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch
import Prelude hiding (State, evalState, get, modify)

import Hoard.BlockFetch.Config (Config (..))
import Hoard.BlockFetch.Events
    ( BlockBatchCompleted (..)
    , BlockFetchFailed (..)
    , BlockFetchRequest (..)
    , BlockFetchStarted (..)
    , BlockReceived (..)
    )
import Hoard.BlockFetch.State (Status, registerRequest, unregisterRequest, waitUntilReadyToFetch)
import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Chan (Chan, readChanBatched)
import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log, withNamespace)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.Types.Cardano (CardanoBlock, CardanoCodecs, CardanoMiniProtocol, CardanoPoint)


miniProtocol
    :: ( Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , Pub :> es
       , Sub :> es
       , State Status :> es
       , Timeout :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Config
    -> CardanoCodecs
    -> Peer
    -> CardanoMiniProtocol
miniProtocol unlift conf codecs peer =
    MiniProtocol
        { miniProtocolNum = blockFetchMiniProtocolNum
        , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
        , miniProtocolStart = StartEagerly
        , miniProtocolRun = InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer $ \_ ->
            let codec = cBlockFetchCodec codecs
                blockFetchClient = client unlift conf peer
                -- tracer = (("[BlockFetch tracer] " <>) . show) >$< Log.asTracer unlift Log.DEBUG
                wrappedPeer = Peer.Effect $ unlift $ withExceptionLogging "BlockFetch" $ do
                    Log.debug "BlockFetch protocol started"
                    pure $ blockFetchClientPeer blockFetchClient
            in  (nullTracer, codec, wrappedPeer)
        }


-- | Create a BlockFetch client that fetches blocks on requests over events.
client
    :: forall es
     . ( Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Log :> es
       , Pub :> es
       , Sub :> es
       , State Status :> es
       , Timeout :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Config
    -> Peer
    -> BlockFetch.BlockFetchClient CardanoBlock CardanoPoint IO ()
client unlift cfg peer =
    BlockFetch.BlockFetchClient $ unlift $ withNamespace "BlockFetchClient" do
        timestamp <- Clock.currentTime
        publish $ BlockFetchStarted {peer, timestamp}
        (inChan, outChan) <- Chan.newChan

        Conc.fork_ $ listen \(req :: BlockFetchRequest) ->
            when (req.peer.id == peer.id) $ Chan.writeChan inChan req

        awaitMessage outChan
  where
    awaitMessage outChan = do
        waitUntilReadyToFetch
        reqs <- readChanBatched cfg.batchTimeoutMicroseconds cfg.batchSize outChan
        registerRequest cfg ((.header) <$> toList reqs)
        Log.info $ "BlockFetch: " <> show peer.address <> ": Received " <> show (length reqs) <> " block fetch requests"
        let points = headerPoint . (.header) <$> reqs
            start = minimum points
            end = maximum points
        pure
            $ BlockFetch.SendMsgRequestRange
                (BlockFetch.ChainRange start end)
                (handleResponse reqs)
            $ client unlift cfg peer

    handleResponse reqs =
        BlockFetch.BlockFetchResponse
            { handleStartBatch =
                pure $ blockReceiver reqs 0
            , handleNoBlocks = unlift do
                unregisterRequest cfg $ (.header) <$> toList reqs
                Log.info "BlockFetch: No blocks returned by peer"
                timestamp <- Clock.currentTime
                for_ reqs \req ->
                    publish
                        BlockFetchFailed
                            { peer
                            , timestamp
                            , header = req.header
                            , errorMessage = "No blocks for point"
                            }
            }

    blockReceiver reqs blockCount =
        BlockFetch.BlockFetchReceiver
            { handleBlock = \block -> unlift do
                timestamp <- Clock.currentTime
                let event =
                        BlockReceived
                            { peer
                            , timestamp
                            , block
                            }
                publish event
                pure $ blockReceiver reqs $ blockCount + 1
            , handleBatchDone = unlift do
                Log.info $ "BlockFetch: Batch done (received " <> show blockCount <> " blocks)"
                unregisterRequest cfg $ (.header) <$> toList reqs
                timestamp <- Clock.currentTime
                publish
                    BlockBatchCompleted
                        { peer
                        , timestamp
                        , blockCount
                        }
            }
