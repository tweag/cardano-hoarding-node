module Hoard.API.Data.BlockViolation
    ( BlockViolation (..)
    , ReceiptInfo (..)
    , SlotDispute (..)
    , blockToViolation
    , groupIntoDisputes
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)

import Data.Map.Strict qualified as Map

import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.Eras (BlockEra, blockToEra)
import Hoard.Data.Header (HeaderReceipt (..))
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)
import Hoard.Data.PoolID (PoolID)
import Hoard.OrphanDetection.Data (BlockClassification (..))


-- | DTO for block violation responses
-- Includes block metadata and receipt information
-- Excludes raw block data which is too complex for JSON serialization at the moment
data BlockViolation = BlockViolation
    { hash :: BlockHash
    , slotNumber :: Int64
    , poolId :: PoolID
    , era :: BlockEra
    , validationStatus :: Text
    , validationReason :: Text
    , firstSeen :: UTCTime
    , classification :: Maybe BlockClassification
    , classifiedAt :: Maybe UTCTime
    , receipts :: [ReceiptInfo] -- List of peers that sent us this block
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)


-- | Receipt information for a block/header
data ReceiptInfo = ReceiptInfo
    { peerId :: ID Peer
    , receivedAt :: UTCTime
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)


-- | A slot dispute: a slot where multiple competing blocks were observed.
-- The canonical winner is included alongside the orphaned block(s).
data SlotDispute = SlotDispute
    { slotNumber :: Int64
    , canonical :: Maybe BlockViolation
    -- ^ The canonical block that won this slot (Nothing if it has never been seen)
    , orphans :: NonEmpty (BlockViolation)
    -- ^ The orphaned block(s) that lost this slot
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)


-- | Convert a Block and its receipts to a BlockViolation DTO
blockToViolation :: Block -> [HeaderReceipt] -> BlockViolation
blockToViolation block receipts =
    BlockViolation
        { hash = block.hash
        , slotNumber = block.slotNumber
        , poolId = block.poolId
        , era = blockToEra block.blockData
        , validationStatus = block.validationStatus
        , validationReason = block.validationReason
        , firstSeen = block.firstSeen
        , classification = block.classification
        , classifiedAt = block.classifiedAt
        , receipts = fmap receiptToInfo receipts
        }
  where
    receiptToInfo receipt =
        ReceiptInfo
            { peerId = receipt.peerId
            , receivedAt = receipt.receivedAt
            }


-- | Group a flat list of classified violations into slot disputes.
-- Each dispute corresponds to a slot where at least one orphan was observed,
-- paired with the canonical block that won (if still present).
--
-- Runs in a single fold over the input list followed by a map traversal.
groupIntoDisputes :: [BlockViolation] -> [SlotDispute]
groupIntoDisputes violations =
    [ SlotDispute {slotNumber = slot, canonical, orphans = o :| os}
    | (slot, (canonical, o : os)) <- Map.toAscList slotMap
    ]
  where
    -- Accumulate into Map slotNumber -> (Maybe canonical, [orphans])
    slotMap :: Map Int64 (Maybe BlockViolation, [BlockViolation])
    slotMap = foldl' step Map.empty violations

    step acc v = case v.classification of
        Just Canonical ->
            Map.insertWith
                (\_ (_, os) -> (Just v, os))
                v.slotNumber
                (Just v, [])
                acc
        Just Orphaned ->
            Map.insertWith
                (\_ (c, os) -> (c, v : os))
                v.slotNumber
                (Nothing, [v])
                acc
        _ -> acc
