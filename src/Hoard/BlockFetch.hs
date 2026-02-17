module Hoard.BlockFetch
    ( -- * Main
      component

      -- * Config
    , Config (..)
    , runConfig

      -- * NodeToNode
    , miniProtocol
    , client

      -- * Events
    , Request (..)
    , RequestStarted (..)
    , BlockReceived (..)
    , RequestFailed (..)
    , BatchCompleted (..)

      -- * Listeners
    , blockFetchStarted
    , blockReceived
    , blockFetchFailed
    , blockBatchCompleted
    ) where

import Control.Tracer (nullTracer)
import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Data.List (maximum, minimum)
import Data.Time (UTCTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.Timeout (Timeout)
import Network.Mux (StartOnDemandOrEagerly (..))
import Ouroboros.Consensus.Block.Abstract (blockSlot, getHeader, headerPoint, unSlotNo)
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
import Prelude hiding (Reader, State, ask, evalState, get, modify, runReader)

import Network.TypedProtocol.Peer.Client qualified as Peer
import Ouroboros.Network.Protocol.BlockFetch.Client qualified as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Data.PoolID (mkPoolID)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Chan (Chan, readChanBatched)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.ConfigPath (ConfigPath, loadYaml)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Metrics.Definitions (recordBlockFetchFailure, recordBlockReceived)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, addEvent, withSpan)
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.Effects.Quota (Quota)
import Hoard.Effects.Verifier (Verifier, verifyBlock)
import Hoard.Types.Cardano (CardanoBlock, CardanoCodecs, CardanoHeader, CardanoMiniProtocol, CardanoPoint)
import Hoard.Types.QuietSnake (QuietSnake (..))

import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Publishing qualified as Sub


---------

-- * Main


---------

component
    :: ( BlockRepo :> es
       , Metrics :> es
       , Quota (ID Peer, Int64) :> es
       , Sub BatchCompleted :> es
       , Sub BlockBatchCompleted :> es
       , Sub BlockFetchFailed :> es
       , Sub BlockFetchStarted :> es
       , Sub BlockReceived :> es
       , Sub RequestFailed :> es
       , Sub RequestStarted :> es
       , Tracing :> es
       , Verifier :> es
       )
    => Component es
component =
    defaultComponent
        { name = "BlockFetch"
        , listeners =
            pure
                [ Sub.listen blockFetchStarted
                , Sub.listen blockReceived
                , Sub.listen blockFetchFailed
                , Sub.listen blockBatchCompleted
                ]
        }


-----------

-- * Config


-----------

data Config = Config
    { batchSize :: Int
    -- ^ Number of block fetch requests to batch
    , batchTimeoutMicroseconds :: Int
    -- ^ Timeout for batching block fetch requests
    , maximumIngressQueue :: Int
    -- ^ Max bytes queued in ingress queue
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { batchSize = 10
            , batchTimeoutMicroseconds = 10_000_000 -- 10 seconds
            , maximumIngressQueue = 393216 -- 384 KiB
            }


data ConfigFile = ConfigFile
    { cardanoProtocols :: CardanoProtocolsConfigFile
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake ConfigFile


data CardanoProtocolsConfigFile = CardanoProtocolsConfigFile
    { blockFetch :: Config
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake CardanoProtocolsConfigFile


runConfig :: (IOE :> es, Reader ConfigPath :> es) => Eff (Reader Config : es) a -> Eff es a
runConfig eff = do
    configPath <- ask
    configFile <- loadYaml @ConfigFile configPath
    runReader configFile.cardanoProtocols.blockFetch eff


-------------
-- NodeToNode
-------------

miniProtocol
    :: forall es
     . ( Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Pub BatchCompleted :> es
       , Pub BlockReceived :> es
       , Pub RequestFailed :> es
       , Pub RequestStarted :> es
       , Reader Config :> es
       , Sub Request :> es
       , Timeout :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> CardanoCodecs
    -> Peer
    -> Eff es CardanoMiniProtocol
miniProtocol unlift' codecs peer = do
    conf <- ask
    pure
        MiniProtocol
            { miniProtocolNum = blockFetchMiniProtocolNum
            , miniProtocolLimits = MiniProtocolLimits conf.maximumIngressQueue
            , miniProtocolStart = StartEagerly
            , miniProtocolRun = InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer $ \_ ->
                let codec = cBlockFetchCodec codecs
                    blockFetchClient = client unlift conf peer
                    tracer = nullTracer
                    wrappedPeer = Peer.Effect $ unlift $ withExceptionLogging "BlockFetch" $ withSpan "block_fetch_protocol" $ do
                        addEvent "protocol_started" []
                        pure $ blockFetchClientPeer blockFetchClient
                in  (tracer, codec, wrappedPeer)
            }
  where
    unlift :: forall x. Eff es x -> IO x
    unlift = unlift'


-- | Create a BlockFetch client that fetches blocks on requests over events.
client
    :: forall es
     . ( Chan :> es
       , Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Pub BatchCompleted :> es
       , Pub BlockReceived :> es
       , Pub RequestFailed :> es
       , Pub RequestStarted :> es
       , Sub Request :> es
       , Timeout :> es
       , Tracing :> es
       )
    => (forall x. Eff es x -> IO x)
    -> Config
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
        let points = headerPoint . (.header) <$> reqs
            start = minimum points
            end = maximum points
        addAttribute "range.start" (show start)
        addAttribute "range.end" (show end)
        pure
            $ BlockFetch.SendMsgRequestRange
                (BlockFetch.ChainRange start end)
                (handleResponse reqs)
            $ client unlift cfg peer

    handleResponse reqs =
        BlockFetch.BlockFetchResponse
            { handleStartBatch =
                pure $ blockReceiver 0
            , handleNoBlocks = unlift $ do
                addEvent "no_blocks_returned" [("request_count", show $ length reqs)]
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
                publish event
                pure $ blockReceiver $ blockCount + 1
            , handleBatchDone = unlift $ do
                addEvent "batch_completed" [("blocks_fetched", show blockCount)]
                timestamp <- Clock.currentTime
                publish
                    $ BatchCompleted
                        { peer
                        , timestamp
                        , blockCount
                        }
            }


---------
-- Events
---------

-- | Events from the BlockFetch mini-protocol.
--
-- BlockFetch is responsible for downloading block bodies after ChainSync
-- has provided the headers.
data RequestStarted = RequestStarted
    { peer :: Peer
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


-- | A request to fetch a single block.
data Request = Request
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    }
    deriving (Eq, Show, Typeable)


data BlockReceived = BlockReceived
    { peer :: Peer
    , timestamp :: UTCTime
    , block :: CardanoBlock
    }
    deriving (Typeable)


data RequestFailed = RequestFailed
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    , errorMessage :: Text
    }
    deriving (Typeable)


data BatchCompleted = BatchCompleted
    { peer :: Peer
    , timestamp :: UTCTime
    , blockCount :: Int
    }
    deriving (Show, Typeable)


------------
-- Listeners
------------

-- | Listener that handles BlockFetch started events
blockFetchStarted :: (Tracing :> es) => RequestStarted -> Eff es ()
blockFetchStarted event = do
    addEvent "block_fetch_started" [("timestamp", show event.timestamp)]


-- | Listener that handles block received events
--
-- Extracts block data and persists it to the database.
blockReceived :: (BlockRepo :> es, Metrics :> es, Quota (ID Peer, Int64) :> es, Tracing :> es, Verifier :> es) => BlockReceived -> Eff es ()
blockReceived event = withSpan "block_received" $ do
    let block = extractBlockData event
        quotaKey = (event.peer.id, block.slotNumber)

    addEvent "block_received" [("slot", show block.slotNumber), ("hash", show block.hash)]
    addAttribute "block.hash" (show block.hash)
    addAttribute "block.slot" (show block.slotNumber)
    addAttribute "peer.id" (show event.peer.id)
    addEvent "block_received" [("slot", show block.slotNumber), ("hash", show block.hash), ("peer_address", show event.peer.address)]

    verifyBlock block >>= \case
        Left _invalidBlock ->
            addEvent "block_invalid" [("slot", show block.slotNumber), ("hash", show block.hash)]
        Right validBlock -> do
            addEvent "block_persisted" [("hash", show block.hash)]
            addAttribute "peer.id" (show event.peer.id)

            Quota.withQuotaCheck quotaKey $ \count status -> do
                case status of
                    Accepted -> do
                        recordBlockReceived
                        BlockRepo.insertBlocks [validBlock]
                        addEvent "block_persisted" [("hash", show block.hash)]
                    Overflow 1 -> do
                        addAttribute "quota.exceeded" "true"
                        -- TODO: Mark the block as equivocating
                        addEvent
                            "quota_exceeded_first"
                            [ ("peer_id", show event.peer.id)
                            , ("slot", show block.slotNumber)
                            , ("count", show count)
                            ]
                    Overflow _ -> do
                        addAttribute "quota.overflow" "true"
                        addEvent
                            "quota_overflow"
                            [ ("peer_id", show event.peer.id)
                            , ("slot", show block.slotNumber)
                            , ("count", show count)
                            ]


-- | Listener that handles block fetch failed events
blockFetchFailed :: (Metrics :> es, Tracing :> es) => RequestFailed -> Eff es ()
blockFetchFailed event = do
    recordBlockFetchFailure
    addEvent "block_fetch_failed" [("error", event.errorMessage)]


-- | Listener that handles block batch completed events
blockBatchCompleted :: (Tracing :> es) => BatchCompleted -> Eff es ()
blockBatchCompleted event = do
    addEvent "block_batch_completed" [("count", show event.blockCount)]


-- | Extract block data from a BlockReceived event.
-- Assumes the block has not been validated.
extractBlockData :: BlockReceived -> Block
extractBlockData BlockReceived {timestamp, block} =
    Block
        { hash = blockHashFromHeader $ getHeader block
        , slotNumber = fromIntegral $ unSlotNo $ blockSlot block
        , poolId = mkPoolID block
        , blockData = block
        , validationStatus = "" -- Block has yet to be validated
        , validationReason = "" -- Block has yet to be validated
        , firstSeen = timestamp
        , classification = Nothing -- Block has yet to be classified
        , classifiedAt = Nothing -- Block has yet to be classified
        }
