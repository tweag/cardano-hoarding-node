module Hoard.Effects.Input
    ( Input (..)
    , input
    , runInputEff
    , runInputConst
    , runInputChan
    , runInputList
    ) where

import Effectful (Eff, Effect, inject, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.State.Static.Shared (evalState, state)
import Effectful.TH (makeEffect)
import Prelude hiding (State, atomically, evalState, get, modify, state)

import Hoard.Effects.Chan (Chan, OutChan)
import Hoard.Effects.Chan qualified as Chan


-- | Receive something from an arbitrary place where the origin of the
-- something in question is not of concern to the logic utilizing it.
data Input a :: Effect where
    Input :: Input a m a


makeEffect ''Input


-- | Run an Input effect by evaluating an `Eff` value.
runInputEff :: (Eff es a) -> Eff (Input a : es) b -> Eff es b
runInputEff eff = interpret_ $ \Input -> eff


-- | Run an Input effect by providing a constant value.
runInputConst :: a -> Eff (Input a : es) b -> Eff es b
runInputConst = runInputEff . pure


-- | Run an Input effect by reading from an `OutChan`.
runInputChan :: (Chan :> es) => OutChan a -> Eff (Input a : es) b -> Eff es b
runInputChan = runInputEff . Chan.readChan


-- | Run an Input effect with the items provided by a NonEmpty list. Loops
-- through the list again once it reaches the end.
runInputList :: NonEmpty a -> Eff (Input a : es) b -> Eff es b
runInputList items =
    evalState items
        . runInputEff
            ( state $ \case
                x :| [] -> (x, items)
                x :| (y : ys) -> (x, y :| ys)
            )
        . inject
