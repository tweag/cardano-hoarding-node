module Hoard.Effects.Output
    ( Output (..)
    , output
    , runOutputEff
    , runOutputDiscard
    , runOutputChan
    , runOutputAsPub
    , runOutputList
    )
where

import Prelude hiding (modify, runState)

import Data.Dynamic qualified as Dyn
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret_, reinterpret_)
import Effectful.State.Static.Local (modify, runState)
import Effectful.TH (makeEffect)
import Hoard.Effects.Chan (Chan, InChan)
import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Pub (Pub, publish)


-- | Output something to whomever cares in a manner that is of no concern for
-- the logic producing the outputted value.
data Output a :: Effect where
    Output :: a -> Output a m ()


makeEffect ''Output


-- | Run an Output effect by discarding all outputted values.
runOutputDiscard :: Eff (Output a : es) x -> Eff es x
runOutputDiscard = interpret_ $ \(Output _) -> pure ()


-- | Run an Output effect using a given Eff-producing function.
runOutputEff :: (a -> Eff es ()) -> Eff (Output a : es) x -> Eff es x
runOutputEff eff = interpret_ $ \(Output x) -> eff x


-- | Run an Output effect by writing to an `InChan`.
runOutputChan :: (Chan :> es) => InChan a -> Eff (Output a : es) x -> Eff es x
runOutputChan inChan = runOutputEff $ Chan.writeChan inChan


-- | Run an Output by publishing values with the `Pub` effect.
runOutputAsPub :: (Typeable a, Pub :> es) => Eff (Output a : es) x -> Eff es x
runOutputAsPub = runOutputEff $ publish . Dyn.toDyn


-- | Collect all values from an Output effect into a list.
runOutputList :: Eff (Output a : es) x -> Eff es (x, [a])
runOutputList =
    fmap (second reverse)
        . reinterpret_
            (runState [])
            (\(Output x) -> modify (x :))
