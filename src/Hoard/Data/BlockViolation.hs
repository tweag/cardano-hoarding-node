module Hoard.Data.BlockViolation
    ( BlockViolation (..)
    , ReceiptInfo (..)
    , blockToViolation
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)

import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.Eras (BlockEra, blockToEra)
import Hoard.Data.Header (HeaderReceipt (..))
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)
import Hoard.Data.PoolID (PoolID)
import Hoard.OrphanDetection.Data (BlockClassification)


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
