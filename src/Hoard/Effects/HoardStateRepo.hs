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
import Hoard.Types.Cardano (ChainPoint)
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
            . fmap (fromMaybe $ TS.immutableTip def)
            . Rel8.runMaybe
            . Rel8.select
            . fmap HoardState.immutableTip
            . Rel8.each
            $ HoardState.schema
    PersistImmutableTip chainPoint ->
        runTransaction "persist_immutable_tip" $ do
            -- to do. avoid deleting by upserting
            TX.statement ()
                . Rel8.run_
                . Rel8.delete
                $ Rel8.Delete
                    { from = HoardState.schema
                    , using = pure ()
                    , deleteWhere = \_ _ -> lit True
                    , returning = Rel8.NoReturning
                    }
            TX.statement ()
                . Rel8.run_
                . Rel8.insert
                $ Rel8.Insert
                    { into = HoardState.schema
                    , rows =
                        Rel8.values
                            [ HoardState.Row
                                { HoardState.immutableTip = lit $ chainPoint
                                }
                            ]
                    , onConflict = Rel8.DoNothing
                    , returning = Rel8.NoReturning
                    }
