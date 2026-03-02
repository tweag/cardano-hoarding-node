module Hoard.Effects.Publishing
    ( Pub
    , Sub
    , listen
    , publish
    , runPubSub
    , runPubWriter
    )
where

import Data.Typeable (typeOf)
import Effectful (Effect)
import Effectful.Dispatch.Dynamic (interpretWith, interpretWith_, interpret_, localSeqUnlift)
import Effectful.TH (makeEffect)
import Effectful.Writer.Static.Shared (Writer, tell)

import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Monitoring.Tracing (SpanContext, Tracing)

import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Monitoring.Tracing qualified as Tracing


data Pub (event :: Type) :: Effect where
    Publish :: event -> Pub event m ()


data Sub (event :: Type) :: Effect where
    Listen :: (event -> m ()) -> Sub event m Void


makeEffect ''Pub
makeEffect ''Sub


-- | Internal wrapper for events with trace context
data TracedEvent event = TracedEvent
    { event :: event
    , publisherSpanContext :: Maybe SpanContext
    }


-- | Runs Pub and Sub effects with an internal channel for a specific event type.
-- Automatically captures span context from the publisher and creates linked spans in listeners.
runPubSub :: forall event es a. (Chan :> es, Tracing :> es, Typeable event) => Eff (Pub event : Sub event : es) a -> Eff es a
runPubSub action = do
    (inChan, _) <- Chan.newChan @(TracedEvent event)

    let handlePub eff = interpretWith_ eff \case
            Publish event -> do
                -- Capture the current span context from the publisher
                publisherSpanContext <- Tracing.getSpanContext
                Chan.writeChan inChan TracedEvent {event, publisherSpanContext}

        handleSub eff = interpretWith eff \env -> \case
            Listen listener -> localSeqUnlift env \unlift -> do
                chan <- Chan.dupChan inChan
                forever do
                    TracedEvent {event, publisherSpanContext} <- Chan.readChan chan
                    case publisherSpanContext of
                        -- If we have a publisher span context, create a linked span
                        Just ctx ->
                            Tracing.withSpanLinked "pubsub.listen" [ctx] $ do
                                Tracing.addAttribute "event.type" (show $ typeOf event :: Text)
                                unlift $ listener event
                        -- Otherwise just run the listener normally
                        Nothing -> unlift $ listener event

    handleSub . handlePub $ action


-- | Handler that uses a provided Writer effect instead of actually publishing.
-- Useful for testing and inspecting what events were published.
runPubWriter :: forall event es a. (Writer [event] :> es) => Eff (Pub event : es) a -> Eff es a
runPubWriter =
    interpret_ \case
        Publish event -> tell [event]
