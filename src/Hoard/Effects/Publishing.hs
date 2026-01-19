module Hoard.Effects.Publishing
    ( Pub
    , Sub
    , listen
    , publish
    , runPubSub
    , runPubWriter
    )
where

import Data.Dynamic qualified as Dyn
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpretWith, interpretWith_, interpret_, localSeqUnlift)
import Prelude hiding (Reader, State, ask, modify, runState)

import Effectful.TH (makeEffect)
import Effectful.Writer.Static.Shared (Writer, tell)
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Chan qualified as Chan


data Pub :: Effect where
    Publish :: (Typeable a) => a -> Pub m ()


data Sub :: Effect where
    Listen :: (Typeable a) => (a -> m ()) -> Sub m Void


makeEffect ''Pub
makeEffect ''Sub


-- | Runs Pub and Sub effects with an internal channel.
runPubSub :: (Chan :> es) => Eff (Pub : Sub : es) a -> Eff es a
runPubSub action = do
    (inChan, _) <- Chan.newChan @Dyn.Dynamic

    let handlePub eff = interpretWith_ eff \case
            Publish event -> Chan.writeChan inChan $ Dyn.toDyn event

        handleSub eff = interpretWith eff \env -> \case
            Listen listener -> localSeqUnlift env \unlift -> do
                chan <- Chan.dupChan inChan
                forever do
                    event <- Chan.readChan chan
                    case Dyn.fromDynamic event of
                        Nothing -> pure ()
                        Just x -> unlift $ listener x

    handleSub . handlePub $ action


-- | Handler that uses a provided Writer effect instead of actually publishing.
-- Useful for testing and inspecting what events were published.
runPubWriter :: (Writer [Dyn.Dynamic] :> es) => Eff (Pub : es) a -> Eff es a
runPubWriter =
    interpret_ \case
        Publish event -> tell [Dyn.toDyn event]
