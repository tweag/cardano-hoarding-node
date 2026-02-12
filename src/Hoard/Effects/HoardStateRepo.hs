module Hoard.Effects.HoardStateRepo
    ( HoardStateRepo
    , getImmutableTip
    , persistImmutableTip
    , runHoardStateRepo
    ) where

import Data.Default (Default (def))
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Rel8 (lit)

import Hasql.Transaction qualified as TX
import Rel8 qualified

import Hoard.Effects.DBRead (DBRead, runQuery)
import Hoard.Effects.DBWrite (DBWrite, runTransaction)
import Hoard.Types.Cardano (ChainPoint)

import Hoard.DB.Schemas.HoardState qualified as HoardState
import Hoard.Types.HoardState qualified as TS


data HoardStateRepo :: Effect where
    GetImmutableTip :: HoardStateRepo m ChainPoint
    PersistImmutableTip :: ChainPoint -> HoardStateRepo m ()


makeEffect ''HoardStateRepo


runHoardStateRepo :: (DBRead :> es, DBWrite :> es) => Eff (HoardStateRepo : es) a -> Eff es a
runHoardStateRepo = interpret_ $ \case
    GetImmutableTip ->
        runQuery "get_immutable_tip"
            . fmap (fromMaybe $ TS.immutableTip def)
            . Rel8.runMaybe
            . Rel8.select
            $ HoardState.immutableTip <$> Rel8.each HoardState.schema
    PersistImmutableTip chainPoint ->
        runTransaction "persist_immutable_tip"
            . TX.statement ()
            . Rel8.run_
            . Rel8.insert
            $ Rel8.Insert
                { into = HoardState.schema
                , rows =
                    Rel8.values
                        [ HoardState.Row
                            { HoardState.unit = lit $ HoardState.Unit
                            , HoardState.immutableTip = lit chainPoint
                            }
                        ]
                , onConflict =
                    Rel8.DoUpdate
                        $ Rel8.Upsert
                            { index = HoardState.unit
                            , predicate = Nothing
                            , set = \new _old -> new
                            , updateWhere = \_ _ -> Rel8.true
                            }
                , returning = Rel8.NoReturning
                }
