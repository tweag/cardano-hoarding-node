module Hoard.DB.Schemas.Blocks
    ( Row (..)
    , schema
    , blockFromRow
    , rowFromBlock
    ) where

import Codec.CBOR.Write qualified as CBOR
import Data.Time (UTCTime)
import Ouroboros.Consensus.Byron.Ledger.Serialisation (encodeByronBlock)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (encodeShelleyBlock)
import Rel8
    ( Column
    , Name
    , Rel8able
    , Result
    , TableSchema
    )

import Hoard.DB.Schema (mkSchema)
import Hoard.Network.Events (CardanoBlock)
import Hoard.Types.Block (Block (..), decodeCardanoBlock)
import Hoard.Types.Eras (BlockEra (..), blockToEra)


data Row f = Row
    { hash :: Column f Text
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


rowFromBlock :: Block -> Row Result
rowFromBlock blk = do
    Row
        { hash = blk.hash
        , slotNumber = blk.slotNumber
        , poolId = blk.poolId
        , blockEra = blockToEra blk.blockData
        , blockData = encode blk.blockData
        , validationStatus = blk.validationStatus
        , validationReason = blk.validationReason
        , isCanonical = blk.isCanonical
        , firstSeen = blk.firstSeen
        }
  where
    encode :: CardanoBlock -> ByteString
    encode =
        CBOR.toStrictByteString . \case
            BlockAllegra b -> encodeShelleyBlock b
            BlockAlonzo b -> encodeShelleyBlock b
            BlockMary b -> encodeShelleyBlock b
            BlockShelley b -> encodeShelleyBlock b
            BlockBabbage b -> encodeShelleyBlock b
            BlockConway b -> encodeShelleyBlock b
            BlockDijkstra b -> encodeShelleyBlock b
            BlockByron b -> encodeByronBlock b
