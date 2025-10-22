module Hoard.Effects.Pub
    ( Pub
    , publish
    , runPub
    , runPubToList
    , runPubWithList
    )
where

import Control.Concurrent.Chan.Unagi (InChan, writeChan)
import Data.Bifunctor (second)
import Data.Typeable (Typeable)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret_, reinterpret_, send)
import Effectful.State.Static.Local (modify, runState)

import Data.Dynamic qualified as Dyn


data Pub :: Effect where
    Publish :: (Typeable a) => a -> Pub m ()


type instance DispatchOf Pub = Dynamic


publish :: (Pub :> es, Typeable a) => a -> Eff es ()
publish = send . Publish


runPub :: (IOE :> es) => InChan Dyn.Dynamic -> Eff (Pub : es) a -> Eff es a
runPub inChan = interpret_ $ \case
    Publish event -> liftIO $ writeChan inChan $ Dyn.toDyn event


runPubToList :: Eff (Pub : es) a -> Eff es (a, [Dyn.Dynamic])
runPubToList =
    fmap (second reverse)
        . reinterpret_
            (runState [])
            (\(Publish event) -> modify (Dyn.toDyn event :))


runPubWithList :: (IOE :> es) => InChan Dyn.Dynamic -> Eff (Pub : es) a -> Eff es (a, [Dyn.Dynamic])
runPubWithList inChan eff = do
    x <- runPub inChan eff
    (_, xs) <- runPubToList eff
    pure (x, xs)
