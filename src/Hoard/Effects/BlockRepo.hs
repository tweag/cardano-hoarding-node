module Hoard.Effects.BlockRepo
    ( BlockRepo
    , insertBlocks
    , hasBlocks
    , runBlockRepo
    ) where

import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as TX
import Rel8 (in_, lit, where_)
import Rel8 qualified

import Hasql.Statement (Statement)
import Hoard.DB.Schemas.Blocks qualified as Blocks
import Hoard.Data.Block (Block (..), encodeCardanoBlock)
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.Eras (blockToEra)
import Hoard.Effects.DBRead (DBRead, runQuery)
import Hoard.Effects.DBWrite (DBWrite, runTransaction)
import Hoard.Types.Cardano (CardanoHeader)


data BlockRepo :: Effect where
    InsertBlocks :: [Block] -> BlockRepo m ()
    HasBlocks :: [CardanoHeader] -> BlockRepo m [Block]


makeEffect ''BlockRepo


runBlockRepo :: (DBRead :> es, DBWrite :> es) => Eff (BlockRepo : es) a -> Eff es a
runBlockRepo = interpret_ $ \case
    InsertBlocks blocks ->
        runTransaction "insert-blocks" $
            insertBlocksTrans blocks
    HasBlocks headers ->
        runQuery "has-blocks" $
            hasBlocksQuery headers


insertBlocksTrans :: [Block] -> Transaction ()
insertBlocksTrans blocks =
    TX.statement ()
        . Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = Blocks.schema
                , rows =
                    Rel8.values $
                        blocks <&> \block ->
                            Blocks.Row
                                { hash = lit block.hash
                                , slotNumber = lit block.slotNumber
                                , poolId = lit block.poolId
                                , blockEra = lit $ blockToEra block.blockData
                                , blockData = lit $ encodeCardanoBlock block.blockData
                                , validationStatus = lit block.validationStatus
                                , validationReason = lit block.validationReason
                                , isCanonical = lit block.isCanonical
                                , firstSeen = lit block.firstSeen
                                }
                , onConflict = Rel8.DoNothing
                , returning = Rel8.NoReturning
                }


hasBlocksQuery :: [CardanoHeader] -> Statement () [Block]
hasBlocksQuery headers =
    fmap (rights . fmap Blocks.blockFromRow)
        . Rel8.run
        . Rel8.select
        $ do
            block <- Rel8.each Blocks.schema
            where_ $
                block.hash `in_` (lit . blockHashFromHeader <$> headers)
            pure block
