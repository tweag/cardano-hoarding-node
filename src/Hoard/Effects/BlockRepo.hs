module Hoard.Effects.BlockRepo
    ( BlockRepo
    , insertBlocks
    , getBlock
    , runBlockRepo
    , runBlockRepoState
    ) where

import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as TX
import Rel8 (lit, where_, (==.))
import Rel8 qualified
import Prelude hiding (State, gets, modify)

import Effectful.State.Static.Shared (State, gets, modify)
import Hasql.Statement (Statement)
import Hoard.DB.Schemas.Blocks (rowFromBlock)
import Hoard.DB.Schemas.Blocks qualified as Blocks
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Effects.DBRead (DBRead, runQuery)
import Hoard.Effects.DBWrite (DBWrite, runTransaction)
import Hoard.Types.Cardano (CardanoHeader)


data BlockRepo :: Effect where
    InsertBlocks :: [Block] -> BlockRepo m ()
    GetBlock :: CardanoHeader -> BlockRepo m (Maybe Block)


makeEffect ''BlockRepo


runBlockRepo :: (DBRead :> es, DBWrite :> es) => Eff (BlockRepo : es) a -> Eff es a
runBlockRepo = interpret_ $ \case
    InsertBlocks blocks ->
        runTransaction "insert-blocks" $
            insertBlocksTrans blocks
    GetBlock header ->
        runQuery "get-block" $
            getBlockQuery header


insertBlocksTrans :: [Block] -> Transaction ()
insertBlocksTrans blocks =
    TX.statement ()
        . Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = Blocks.schema
                , rows = Rel8.values $ rowFromBlock <$> blocks
                , onConflict = Rel8.DoNothing
                , returning = Rel8.NoReturning
                }


getBlockQuery :: CardanoHeader -> Statement () (Maybe Block)
getBlockQuery header =
    fmap (extractSingleBlock . rights . fmap Blocks.blockFromRow)
        . Rel8.run
        . Rel8.select
        $ do
            block <- Rel8.each Blocks.schema
            where_ $
                block.hash ==. (lit $ blockHashFromHeader header)
            pure block
  where
    -- The unique constraint over the `hash` column ensures we get either 1 or
    -- 0 rows from the above query.
    extractSingleBlock = \case
        [x] -> Just x
        _ -> Nothing


runBlockRepoState :: (State [Block] :> es) => Eff (BlockRepo : es) a -> Eff es a
runBlockRepoState = interpret_ \case
    InsertBlocks blocks -> modify $ (blocks <>)
    GetBlock header -> gets $ find ((blockHashFromHeader header ==) . (.hash))
