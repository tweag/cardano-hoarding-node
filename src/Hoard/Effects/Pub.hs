module Hoard.Effects.Pub
    ( Pub
    , publish
    , runPub
    , runPubToList
    , runPubWithList
    )
where

import Prelude hiding (modify, runState)

import Data.Dynamic qualified as Dyn
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret_, reinterpret_, send)
import Effectful.State.Static.Local (modify, runState)
import Hoard.Effects.Chan (Chan, InChan)
import Hoard.Effects.Chan qualified as Chan


data Pub :: Effect where
    Publish :: (Typeable a) => a -> Pub m ()


type instance DispatchOf Pub = Dynamic


publish :: (Pub :> es, Typeable a) => a -> Eff es ()
publish = send . Publish


runPub :: (Chan :> es) => InChan Dyn.Dynamic -> Eff (Pub : es) a -> Eff es a
runPub inChan = interpret_ $ \case
    Publish event -> Chan.writeChan inChan $ Dyn.toDyn event


runPubToList :: Eff (Pub : es) a -> Eff es (a, [Dyn.Dynamic])
runPubToList =
    fmap (second reverse)
        . reinterpret_
            (runState [])
            (\(Publish event) -> modify (Dyn.toDyn event :))


runPubWithList :: (Chan :> es) => InChan Dyn.Dynamic -> Eff (Pub : es) a -> Eff es (a, [Dyn.Dynamic])
runPubWithList inChan eff = do
    x <- runPub inChan eff
    (_, xs) <- runPubToList eff
    pure (x, xs)
