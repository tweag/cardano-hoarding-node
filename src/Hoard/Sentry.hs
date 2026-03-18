module Hoard.Sentry
    ( component
    , DuplicateBlocks (..)
    , AdversarialBehavior (..)
    , AdversarialSeverity (..)
    , ReceivedBlockOutsideRequestedRange (..)
    , runDuplicateBlocksReader

      -- * Guards
    , duplicateBlockGuard
    , DuplicateBlocksKey (..)
    , receivedBlockIsOutsideRequestedRangeGuard
    , headerBlockMismatchGuard
    , ReceivedMismatchingBlock (..)

      -- * Config
    , Config (..)
    , AdversarialThresholds (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import Data.UUID (UUID)
import Effectful.Concurrent.STM (Concurrent, atomically)
import Effectful.Reader.Static (Reader, asks, runReader)
import Ouroboros.Consensus.Block (SlotNo (..), blockMatchesHeader, blockSlot)
import StmContainers.Map (Map)

import Ouroboros.Consensus.Block.Abstract qualified as Block
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch
import StmContainers.Map qualified as Map

import Atelier.Effects.Monitoring.Tracing
    ( Tracing
    , addAttribute
    , withSpan
    )
import Atelier.Effects.Publishing (Pub, Sub, publish)
import Atelier.Effects.Quota (Quota)
import Atelier.Types.QuietSnake (QuietSnake (..))
import Hoard.Component (Component (..), defaultComponent)
import Hoard.Data.BlockHash (BlockHash, mkBlockHash)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Events.BlockFetch (BlockReceived (..))
import Hoard.Types.Cardano (CardanoBlock)
import Prelude hiding (Map)

import Atelier.Effects.Publishing qualified as Sub
import Atelier.Effects.Quota qualified as Quota


component
    :: ( Pub AdversarialBehavior :> es
       , Pub ReceivedBlockOutsideRequestedRange :> es
       , Quota DuplicateBlocksKey :> es
       , Reader Config :> es
       , Sub BlockReceived :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "Sentry"
        , listeners =
            pure
                [ Sub.listen_ duplicateBlockGuard
                , Sub.listen_ receivedBlockIsOutsideRequestedRangeGuard
                ]
        }


newtype DuplicateBlocks = DuplicateBlocks
    { duplicateBlocks :: Map (ID Peer, UUID, BlockHash) Word
    }


runDuplicateBlocksReader :: (Concurrent :> es) => Eff (Reader DuplicateBlocks : es) a -> Eff es a
runDuplicateBlocksReader eff = do
    m <- atomically Map.new
    runReader (DuplicateBlocks m) eff


duplicateBlockGuard
    :: ( Pub AdversarialBehavior :> es
       , Quota DuplicateBlocksKey :> es
       , Reader Config :> es
       , Tracing :> es
       )
    => BlockReceived -> Eff es ()
duplicateBlockGuard event = withSpan "sentry.duplicate_block_guard" do
    addAttribute "peer.address" event.peer.address
    addAttribute "request.id" $ show @Text event.requestId
    let blockHash = mkBlockHash event.block
    let key =
            DuplicateBlocksKey
                { peerId = event.peer.id
                , requestId = event.requestId
                , hash = blockHash
                }
    cfg <- asks @Config (.duplicateBlocks)
    Quota.withQuotaCheck key (classify cfg) $ \case
        Nothing -> addAttribute @Text "result" "none"
        Just Critical -> do
            addAttribute @Text "result" "warning"
            publish
                $ AdversarialBehavior
                    { peer = event.peer
                    , description = "exceeded duplicate block critical threshold"
                    , severity = Critical
                    }
        Just Minor -> do
            addAttribute @Text "result" "critical"
            publish
                $ AdversarialBehavior
                    { peer = event.peer
                    , description = "exceeded duplicate block warning threshold"
                    , severity = Minor
                    }
  where
    classify cfg c
        | c > fromIntegral cfg.criticalThreshold = Just Critical
        | c > fromIntegral cfg.warningThreshold = Just Minor
        | otherwise = Nothing


data DuplicateBlocksKey = DuplicateBlocksKey
    { peerId :: ID Peer
    , requestId :: UUID
    , hash :: BlockHash
    }
    deriving (Eq, Generic, Hashable, Ord)


receivedBlockIsOutsideRequestedRangeGuard
    :: ( Pub AdversarialBehavior :> es
       , Pub ReceivedBlockOutsideRequestedRange :> es
       , Tracing :> es
       )
    => BlockReceived -> Eff es ()
receivedBlockIsOutsideRequestedRangeGuard event =
    when (blockNo < startNo || blockNo > endNo) $ withSpan "sentry.unrequested_block_guard" do
        publish
            AdversarialBehavior
                { peer = event.peer
                , severity = Minor
                , description = "returned block outside of requested range"
                }
        publish
            ReceivedBlockOutsideRequestedRange
                { peer = event.peer
                , block = event.block
                }
  where
    BlockFetch.ChainRange start end = event.range
    SlotNo blockNo = Block.blockSlot event.block
    startNo = pointToNo start
    endNo = pointToNo end
    pointToNo = \case
        Block.GenesisPoint -> 0
        Block.BlockPoint (SlotNo n) _ -> n


headerBlockMismatchGuard
    :: ( Pub AdversarialBehavior :> es
       , Pub ReceivedMismatchingBlock :> es
       , Quota HashMismatchKey :> es
       , Reader Config :> es
       , Tracing :> es
       )
    => BlockReceived -> Eff es ()
headerBlockMismatchGuard event =
    withSpan "sentry.header_block_mismatch_guard" do
        cfg <- asks (.hashMismatch)
        let classify hits
                | hits > cfg.criticalThreshold = Just Critical
                | hits > cfg.warningThreshold = Just Minor
                | otherwise = Nothing
        case event.headerWithSameSlotNumber of
            Just header | not (blockMatchesHeader header event.block) -> do
                Quota.withQuotaCheck quotaKey (classify . fromIntegral) \case
                    Just severity -> do
                        addAttribute "result" $ show @Text severity
                        publish
                            AdversarialBehavior
                                { peer = event.peer
                                , severity
                                , description = "exceeded header-block hash mismatch " <> show severity <> " threshold"
                                }
                        publish
                            ReceivedMismatchingBlock
                                { peer = event.peer
                                , block = event.block
                                }
                    Nothing -> do
                        addAttribute @Text "result" "None"
            _ -> pure ()
  where
    quotaKey =
        HashMismatchKey
            { peerId = event.peer.id
            , slotNumber = unSlotNo $ blockSlot event.block
            , requestId = event.requestId
            }


data HashMismatchKey = HashMismatchKey
    { peerId :: ID Peer
    , slotNumber :: Word64
    , requestId :: UUID
    }
    deriving (Eq, Generic, Hashable, Ord)


data ReceivedMismatchingBlock = ReceivedMismatchingBlock
    { peer :: Peer
    , block :: CardanoBlock
    }
    deriving (Eq, Show)


data AdversarialSeverity
    = Minor
    | Critical
    deriving (Bounded, Enum, Eq, Show)


data AdversarialBehavior = AdversarialBehavior
    { peer :: Peer
    , severity :: AdversarialSeverity
    , description :: Text
    }
    deriving (Eq, Show)


data ReceivedBlockOutsideRequestedRange = ReceivedBlockOutsideRequestedRange
    { peer :: Peer
    , block :: CardanoBlock
    }
    deriving (Eq, Show)


data Config = Config
    { duplicateBlocks :: AdversarialThresholds
    , hashMismatch :: AdversarialThresholds
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { duplicateBlocks =
                AdversarialThresholds
                    { warningThreshold = 1
                    , criticalThreshold = 20
                    }
            , hashMismatch =
                AdversarialThresholds
                    { warningThreshold = 0
                    , criticalThreshold = 5
                    }
            }


data AdversarialThresholds = AdversarialThresholds
    { warningThreshold :: Word
    -- ^ Threshold before the peer is considered to exhibit adversarial behavior.
    , criticalThreshold :: Word
    -- ^ Threshold before the peer is considered adequately adversarial to warrant major action.
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake AdversarialThresholds
