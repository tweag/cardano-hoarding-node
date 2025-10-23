module Hoard.Effects.Sub
    ( Sub
    , listen
    , runSub
    )
where

import Control.Concurrent.Chan.Unagi (InChan, dupChan, readChan)
import Control.Monad (forever)
import Data.Typeable (Typeable)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, send)

import Data.Dynamic qualified as Dyn


data Sub :: Effect where
    Listen :: (Typeable a) => (a -> m ()) -> Sub m ()


type instance DispatchOf Sub = Dynamic


listen :: (Sub :> es, Typeable a) => (a -> Eff es ()) -> Eff es ()
listen = send . Listen


runSub :: (IOE :> es) => InChan Dyn.Dynamic -> Eff (Sub : es) a -> Eff es a
runSub inChan = interpret $ \env -> \case
    Listen listener -> localSeqUnlift env $ \unlift -> do
        chan <- liftIO $ dupChan inChan
        forever $ do
            event <- liftIO $ readChan chan
            case Dyn.fromDynamic event of
                Nothing -> pure ()
                Just x -> unlift $ listener x
