module Hoard.BlockFetch.NodeToNode (miniProtocol, client) where

import Data.List (maximum, minimum)
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.QSem (signalQSem, waitQSem)
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
import Prelude hiding (Reader, State, asks, evalState)

import Hoard.BlockFetch.Config (Config (..), Handles (..))
import Hoard.BlockFetch.Events
    ( BlockBatchCompleted (..)
    , BlockFetchFailed (..)
    , BlockFetchRequest (..)
    , BlockFetchStarted (..)
    , BlockReceived (..)
    )
import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode.Config qualified as NodeToNode
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Types.Cardano (CardanoBlock, CardanoCodecs, CardanoMiniProtocol, CardanoPoint)


miniProtocol
    :: ( Clock :> es
       , Concurrent :> es
       , Log :> es
       , Pub :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Config
    -> Handles
    -> NodeToNode.Config (Eff es)
    -> CardanoCodecs
    -> Peer
    -> CardanoMiniProtocol
miniProtocol unlift conf handles n2nConf codecs peer =
    MiniProtocol
        { miniProtocolNum = blockFetchMiniProtocolNum
        , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
        , miniProtocolStart = StartEagerly
        , miniProtocolRun = InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer $ \_ ->
            let codec = cBlockFetchCodec codecs
                blockFetchClient = client unlift handles n2nConf peer
                tracer = (("[BlockFetch tracer] " <>) . show) >$< Log.asTracer unlift Log.DEBUG
                wrappedPeer = Peer.Effect $ unlift $ withExceptionLogging "BlockFetch" $ do
                    Log.debug "BlockFetch protocol started"
                    pure $ blockFetchClientPeer blockFetchClient
            in  (tracer, codec, wrappedPeer)
        }


-- | Create a BlockFetch client that fetches blocks on request over a channel.
--
-- This client:
client
    :: forall es
     . ( Clock :> es
       , Concurrent :> es
       , Log :> es
       , Pub :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Handles
    -> NodeToNode.Config (Eff es)
    -> Peer
    -> BlockFetch.BlockFetchClient CardanoBlock CardanoPoint IO ()
client unlift handles n2nConf peer =
    BlockFetch.BlockFetchClient $ unlift $ do
        timestamp <- Clock.currentTime
        publish $ BlockFetchStarted {peer, timestamp}
        Log.debug "BlockFetch: Published BlockFetchStarted event"
        Log.debug "BlockFetch: Starting client, awaiting block download requests"
        awaitMessage
  where
    awaitMessage :: Eff es (BlockFetch.BlockFetchRequest CardanoBlock CardanoPoint IO ())
    awaitMessage = do
        waitQSem handles.qSem
        reqs <- n2nConf.awaitBlockFetchRequests
        Log.info $ "BlockFetch: Received " <> show (length reqs) <> " block fetch requests"
        let points = headerPoint . (.header) <$> reqs
            start = minimum points
            end = maximum points
        pure
            $ BlockFetch.SendMsgRequestRange
                (BlockFetch.ChainRange start end)
                (handleResponse reqs)
            $ client unlift handles n2nConf peer

    handleResponse reqs =
        BlockFetch.BlockFetchResponse
            { handleStartBatch =
                pure $ blockReceiver 0
            , handleNoBlocks = unlift $ do
                signalQSem handles.qSem
                timestamp <- Clock.currentTime
                for_ reqs \req ->
                    publish $
                        BlockFetchFailed
                            { peer
                            , timestamp
                            , header = req.header
                            , errorMessage = "No blocks for point"
                            }
            }

    blockReceiver blockCount =
        BlockFetch.BlockFetchReceiver
            { handleBlock = \block -> unlift $ do
                timestamp <- Clock.currentTime
                let event =
                        BlockReceived
                            { peer
                            , timestamp
                            , block
                            }
                n2nConf.emitFetchedBlock event
                publish event
                pure $ blockReceiver $ blockCount + 1
            , handleBatchDone = unlift $ do
                signalQSem handles.qSem
                timestamp <- Clock.currentTime
                publish $
                    BlockBatchCompleted
                        { peer
                        , timestamp
                        , blockCount
                        }
            }
