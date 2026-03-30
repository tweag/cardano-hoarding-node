module Hoard.Effects.HoardStateRepo
    ( HoardStateRepo
    , getImmutableTip
    , persistImmutableTip
    , runHoardStateRepo
    ) where

import Data.Default (Default (def))
import Effectful (Effect)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Rel8 (lit)

import Rel8 qualified

import Hoard.Effects.DB (DBRead, DBWrite, insert_, select, transact)
import Hoard.Types.Cardano (ChainPoint)

import Hoard.DB.Schemas.HoardState qualified as HoardState
import Hoard.Types.HoardState qualified as TS


data HoardStateRepo :: Effect where
    GetImmutableTip :: HoardStateRepo m ChainPoint
    PersistImmutableTip :: ChainPoint -> HoardStateRepo m ()


makeEffect ''HoardStateRepo


runHoardStateRepo :: (DBRead :> es, DBWrite :> es) => Eff (HoardStateRepo : es) a -> Eff es a
runHoardStateRepo = interpret_ \case
    GetImmutableTip -> do
        rows <- select "get_immutable_tip" $ HoardState.immutableTip <$> Rel8.each HoardState.schema
        pure $ fromMaybe (TS.immutableTip def) $ listToMaybe rows
    PersistImmutableTip chainPoint ->
        transact "persist_immutable_tip"
            $ insert_
                Rel8.Insert
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
