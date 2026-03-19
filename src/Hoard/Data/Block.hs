module Hoard.Data.Block (Block (..)) where

import Data.Time (UTCTime)

import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.PoolID (PoolID)
import Hoard.OrphanDetection.Data (BlockClassification)
import Hoard.Types.Cardano (CardanoBlock)


data Block = Block
    { hash :: BlockHash
    , slotNumber :: Int64
    , poolId :: PoolID
    , blockData :: CardanoBlock
    , validationStatus :: Text
    , validationReason :: Text
    , firstSeen :: UTCTime
    , classification :: Maybe BlockClassification
    , classifiedAt :: Maybe UTCTime
    }
    deriving stock (Eq, Generic)
