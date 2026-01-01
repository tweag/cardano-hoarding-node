module Hoard.DB.Schemas.Blocks
    ( Row (..)
    , schema
    , blockFromRow
    , rowFromBlock
    ) where

import Data.Time (UTCTime)
import Rel8
    ( Column
    , Expr
    , Name
    , Rel8able
    , Result
    , TableSchema
    , lit
    )

import Hoard.DB.Schema (mkSchema)
import Hoard.Data.Block (Block (..), decodeCardanoBlock, encodeCardanoBlock)
import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.Eras (BlockEra (..), blockToEra)


data Row f = Row
    { hash :: Column f BlockHash
    , slotNumber :: Column f Int64
    , poolId :: Column f Text
    , blockEra :: Column f BlockEra
    , blockData :: Column f ByteString
    , validationStatus :: Column f Text
    , validationReason :: Column f Text
    , isCanonical :: Column f Bool
    , firstSeen :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)


deriving instance Eq (Row Result)
deriving instance Show (Row Result)


schema :: TableSchema (Row Name)
schema = mkSchema "blocks"


blockFromRow :: Row Result -> Either Text Block
blockFromRow row = do
    bd <- decodeCardanoBlock row.blockEra row.blockData
    pure $
        Block
            { hash = row.hash
            , slotNumber = row.slotNumber
            , poolId = row.poolId
            , blockData = bd
            , validationStatus = row.validationStatus
            , validationReason = row.validationReason
            , isCanonical = row.isCanonical
            , firstSeen = row.firstSeen
            }


rowFromBlock :: Block -> Row Expr
rowFromBlock blk = do
    Row
        { hash = lit blk.hash
        , slotNumber = lit blk.slotNumber
        , poolId = lit blk.poolId
        , blockEra = lit $ blockToEra blk.blockData
        , blockData = lit $ encodeCardanoBlock blk.blockData
        , validationStatus = lit blk.validationStatus
        , validationReason = lit blk.validationReason
        , isCanonical = lit blk.isCanonical
        , firstSeen = lit blk.firstSeen
        }
