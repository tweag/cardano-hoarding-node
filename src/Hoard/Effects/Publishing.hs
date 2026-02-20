module Hoard.Effects.Publishing
    ( Pub
    , Sub
    , listen
    , publish
    , runPubSub
    , runPubWriter
    )
where

import Data.Typeable (tyConModule, typeOf, typeRep, typeRepTyCon)
import Effectful (Effect)
import Effectful.Dispatch.Dynamic (interpretWith, interpretWith_, interpret_, localSeqUnlift)
import Effectful.TH (makeEffect)
import Effectful.Writer.Static.Shared (Writer, tell)
import GHC.Stack (srcLocFile, srcLocStartLine)
import Relude.Extra (typeName)

import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Monitoring.Tracing (SpanContext, Tracing, addAttribute, withSpan)

import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Monitoring.Tracing qualified as Tracing


data Pub (event :: Type) :: Effect where
    Pub :: CallStack -> event -> Pub event m ()


data Sub (event :: Type) :: Effect where
    Sub :: CallStack -> (event -> m ()) -> Sub event m Void


makeEffect ''Pub
makeEffect ''Sub


-- | Internal wrapper for events with trace context
data TracedEvent event = TracedEvent
    { event :: event
    , stack :: CallStack
    , publisherSpanContext :: Maybe SpanContext
    }


publish :: (HasCallStack, Pub event :> es) => event -> Eff es ()
publish event = pub callStack event


listen :: (HasCallStack, Sub event :> es) => (event -> Eff es ()) -> Eff es Void
listen listener = sub callStack listener


-- | Runs Pub and Sub effects with an internal channel for a specific event type.
runPubSub
    :: forall event es a
     . ( Chan :> es
       , Tracing :> es
       , Typeable event
       )
    => Eff (Pub event : Sub event : es) a -> Eff es a
runPubSub action = withSpan ("runPubSub." <> typeName @event) do
    addAttribute "event.name" name
    addAttribute "event.module" $ toText moduleName
    (inChan, _) <- Chan.newChan @(TracedEvent event)

    let handlePub eff = interpretWith_ eff \case
            Pub stack event -> withSpan "pub_sub.publish" do
                addAttribute "event.name" name
                addAttribute "event.module" $ toText moduleName
                addAttribute "event.source" $ callSite stack
                -- Capture the current span context from the publisher
                publisherSpanContext <- Tracing.getSpanContext
                Chan.writeChan inChan TracedEvent {event, publisherSpanContext, stack}

        handleSub eff = interpretWith eff \env -> \case
            Sub listenerStack listener -> localSeqUnlift env \unlift -> do
                chan <- Chan.dupChan inChan
                forever $ withSpan "pub_sub.event_received" do
                    event <- Chan.readChan chan

                    addAttribute "event.name" $ toText moduleName <> "." <> name
                    addAttribute "event.source" $ callSite event.stack
                    addAttribute "event.destination" $ callSite listenerStack

                    case event.publisherSpanContext of
                        -- If we have a publisher span context, create a linked span
                        Just ctx ->
                            Tracing.withSpanLinked "pubsub.listen" [ctx] $ do
                                Tracing.addAttribute "event.type" (show $ typeOf event.event :: Text)
                                unlift $ listener event.event
                        -- Otherwise just run the listener normally
                        Nothing -> unlift $ listener event.event

    handleSub . handlePub $ action
  where
    moduleName = tyConModule $ typeRepTyCon $ typeRep $ Proxy @event
    name = typeName @event
    callSite :: CallStack -> Text
    callSite stack = case getCallStack stack of
        (_, loc) : _ -> toText $ srcLocFile loc <> ":" <> show (srcLocStartLine loc)
        [] -> "unknown callsite"


-- | Handler that uses a provided Writer effect instead of actually publishing.
-- Useful for testing and inspecting what events were published.
runPubWriter :: forall event es a. (Writer [event] :> es) => Eff (Pub event : es) a -> Eff es a
runPubWriter =
    interpret_ \case
        Pub _ event -> tell [event]
