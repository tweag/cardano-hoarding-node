module Hoard.Effects.HoardStateRepo
    ( HoardStateRepo
    , getImmutableTip
    , persistImmutableTip
    , runHoardStateRepo
    ) where

import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Hasql.Transaction qualified as TX
import Rel8 (lit)
import Rel8 qualified

import Data.Default (Default (def))
import Hoard.DB.Schemas.HoardState qualified as HoardState
import Hoard.Effects.DBWrite (DBWrite, runTransaction)
import Hoard.Types.HoardState (ChainPoint)
import Hoard.Types.HoardState qualified as TS


data HoardStateRepo :: Effect where
    GetImmutableTip :: HoardStateRepo m ChainPoint
    PersistImmutableTip :: ChainPoint -> HoardStateRepo m ()


makeEffect ''HoardStateRepo


runHoardStateRepo :: (DBWrite :> es) => Eff (HoardStateRepo : es) a -> Eff es a
runHoardStateRepo = interpret_ $ \case
    GetImmutableTip ->
        runTransaction "get_immutable_tip"
            . TX.statement ()
            . fmap
                ( \case
                    [] -> TS.immutableTip def
                    [row] -> row
                    _ -> error "`hoard_state` table should not be able to have more than 1 row."
                )
            . Rel8.run
            . Rel8.select
            . fmap HoardState.immutableTip
            . Rel8.each
            $ HoardState.schema
    PersistImmutableTip chainPoint ->
        runTransaction "persist_immutable_tip"
            . TX.statement ()
            . Rel8.run_
            $ Rel8.update
                Rel8.Update
                    { target = HoardState.schema
                    , from = pure ()
                    , updateWhere = \() _row -> lit True
                    , set = \() row ->
                        row
                            { HoardState.immutableTip = lit $ chainPoint
                            }
                    , returning = Rel8.NoReturning
                    }
