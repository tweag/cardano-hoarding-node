module Hoard.Effects.Publishing
    ( Pub
    , Sub
    , listen
    , publish
    , runPubSub
    , runPubWriter
    )
where

import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpretWith, interpretWith_, interpret_, localSeqUnlift)
import Effectful.TH (makeEffect)
import Effectful.Writer.Static.Shared (Writer, tell)
import Prelude hiding (Reader, State, ask, modify, runState)

import Hoard.Effects.Chan (Chan)

import Hoard.Effects.Chan qualified as Chan


data Pub (event :: Type) :: Effect where
    Publish :: event -> Pub event m ()


data Sub (event :: Type) :: Effect where
    Listen :: (event -> m ()) -> Sub event m Void


makeEffect ''Pub
makeEffect ''Sub


-- | Runs Pub and Sub effects with an internal channel for a specific event type.
runPubSub :: forall event es a. (Chan :> es) => Eff (Pub event : Sub event : es) a -> Eff es a
runPubSub action = do
    (inChan, _) <- Chan.newChan @event

    let handlePub eff = interpretWith_ eff \case
            Publish event -> Chan.writeChan inChan event

        handleSub eff = interpretWith eff \env -> \case
            Listen listener -> localSeqUnlift env \unlift -> do
                chan <- Chan.dupChan inChan
                forever do
                    event <- Chan.readChan chan
                    unlift $ listener event

    handleSub . handlePub $ action


-- | Handler that uses a provided Writer effect instead of actually publishing.
-- Useful for testing and inspecting what events were published.
runPubWriter :: forall event es a. (Writer [event] :> es) => Eff (Pub event : es) a -> Eff es a
runPubWriter =
    interpret_ \case
        Publish event -> tell [event]
