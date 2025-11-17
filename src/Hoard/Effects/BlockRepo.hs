module Hoard.Effects.BlockRepo
    ( BlockRepo
    , insertBlocks
    , diffHeaders
    , runBlockRepo
    ) where

import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as TX
import Rel8 (lit)
import Rel8 qualified

import Hoard.DB.Schemas.Blocks qualified as Blocks
import Hoard.Effects.DBRead (DBRead)
import Hoard.Effects.DBWrite (DBWrite, runTransaction)
import Hoard.Network.Events (Header)
import Hoard.Types.Block (Block (..), encodeCardanoBlock)
import Hoard.Types.Eras (blockToEra)


data BlockRepo :: Effect where
    InsertBlocks :: Set Block -> BlockRepo m ()
    DiffHeaders :: Set Header -> BlockRepo m (Set Header)


makeEffect ''BlockRepo


runBlockRepo :: (DBRead :> es, DBWrite :> es) => Eff (BlockRepo : es) a -> Eff es a
runBlockRepo = interpret_ $ \case
    InsertBlocks blocks ->
        runTransaction "insert-blocks" $
            insertBlocksTrans blocks
    DiffHeaders headers ->
        runTransaction "has-blocks" $
            diffHeadersTrans headers


insertBlocksTrans :: Set Block -> Transaction ()
insertBlocksTrans blocks =
    TX.statement ()
        . Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = Blocks.schema
                , rows =
                    Rel8.values $
                        toList blocks <&> \block ->
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


diffHeadersTrans :: Set Header -> Transaction (Set Header)
diffHeadersTrans headers =
    Rel8.run $
        Rel8.select $ do
            block <- Rel8.each Blocks.schema
            where_ $
                block.hash `Rel8.in_` (lit . Header.hash <$> headers)
