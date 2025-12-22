module Hoard.Effects.Sub
    ( Sub
    , listen
    , runSub
    )
where

import Data.Dynamic qualified as Dyn
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpretWith, localSeqUnlift, send)
import Effectful.Reader.Static (Reader, ask)
import Prelude hiding (Reader, ask)

import Hoard.Effects.Chan (Chan, InChan)
import Hoard.Effects.Chan qualified as Chan


data Sub :: Effect where
    Listen :: (Typeable a) => (a -> m ()) -> Sub m Void


type instance DispatchOf Sub = Dynamic


listen :: (Sub :> es, Typeable a) => (a -> Eff es ()) -> Eff es Void
listen = send . Listen


runSub :: (Chan :> es, Reader (InChan Dyn.Dynamic) :> es) => Eff (Sub : es) a -> Eff es a
runSub eff = do
    inChan <- ask
    interpretWith eff \env -> \case
        Listen listener -> localSeqUnlift env \unlift -> do
            chan <- Chan.dupChan inChan
            forever do
                event <- Chan.readChan chan
                case Dyn.fromDynamic event of
                    Nothing -> pure ()
                    Just x -> unlift $ listener x
