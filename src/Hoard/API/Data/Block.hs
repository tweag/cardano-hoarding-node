module Hoard.API.Data.Block
    ( Block (..)
    , fromBlock
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)

import Atelier.Types.QuietSnake (QuietSnake (..))
import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.Eras (BlockEra)
import Hoard.Data.PoolID (PoolID)
import Hoard.Data.Serialized (Serialized, serializeBlock)
import Hoard.OrphanDetection.Data (BlockClassification)
import Hoard.Types.Cardano (CardanoBlock)

import Hoard.Data.Block qualified as Block
import Hoard.Data.Eras qualified as Era


-- | API stable representation of collected blocks.
data Block = Block
    { hash :: BlockHash
    , slotNumber :: Int64
    , poolId :: PoolID
    , blockEra :: BlockEra
    , blockData :: Serialized CardanoBlock
    , validationStatus :: Text
    , validationReason :: Text
    , firstSeen :: UTCTime
    , classification :: Maybe BlockClassification
    , classifiedAt :: Maybe UTCTime
    }
    deriving stock (Eq, Generic)


fromBlock :: Block.Block -> Block
fromBlock b =
    Block
        { hash = b.hash
        , slotNumber = b.slotNumber
        , poolId = b.poolId
        , blockEra = Era.blockToEra b.blockData
        , blockData = serializeBlock b.blockData
        , validationStatus = b.validationStatus
        , validationReason = b.validationReason
        , firstSeen = b.firstSeen
        , classification = b.classification
        , classifiedAt = b.classifiedAt
        }


deriving via QuietSnake Block instance (FromJSON Block)
deriving via QuietSnake Block instance (ToJSON Block)
