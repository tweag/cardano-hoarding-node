module Hoard.Effects.Publishing
    ( Pub
    , Sub
    , listen
    , publish
    , runPubSub
    , runPubWriter
    )
where

import Data.Typeable (tyConModule, typeRep, typeRepTyCon)
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpretWith, interpretWith_, interpret_, localSeqUnlift)
import Effectful.TH (makeEffect)
import Effectful.Writer.Static.Shared (Writer, tell)
import GHC.Stack (srcLocFile, srcLocStartLine)
import Relude.Extra (typeName)
import Prelude hiding (Reader, State, ask, modify, runState)

import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, withSpan)

import Hoard.Effects.Chan qualified as Chan


data Pub (event :: Type) :: Effect where
    Pub :: CallStack -> event -> Pub event m ()


data Sub (event :: Type) :: Effect where
    Sub :: CallStack -> (event -> m ()) -> Sub event m Void


makeEffect ''Pub
makeEffect ''Sub


publish :: (HasCallStack, Pub event :> es) => event -> Eff es ()
publish event = pub callStack event


listen :: (HasCallStack, Sub event :> es) => (event -> Eff es ()) -> Eff es Void
listen listener = sub callStack listener


-- | Runs Pub and Sub effects with an internal channel for a specific event type.
runPubSub
    :: forall event es a
     . ( Chan :> es
       , Show event
       , Tracing :> es
       , Typeable event
       )
    => Eff (Pub event : Sub event : es) a -> Eff es a
runPubSub action = withSpan ("runPubSub." <> typeName @event) do
    addAttribute "event.name" name
    addAttribute "event.module" $ toText moduleName
    (inChan, _) <- Chan.newChan @(CallStack, event)

    let handlePub eff = interpretWith_ eff \case
            Pub stack event -> withSpan "publish" do
                addAttribute "event.name" name
                addAttribute "event.module" $ toText moduleName
                addAttribute "event.source" $ callSite stack
                addAttribute "event.body" $ show @Text event
                Chan.writeChan inChan (stack, event)

        handleSub eff = interpretWith eff \env -> \case
            Sub stack listener -> withSpan "listen" $ localSeqUnlift env \unlift -> do
                chan <- Chan.dupChan inChan
                forever $ withSpan "event_received" do
                    (eventSourceStack, event) <- Chan.readChan chan
                    addAttribute "event.name" name
                    addAttribute "event.module" $ toText moduleName
                    addAttribute "event.body" $ show @Text event
                    addAttribute "event.source" $ callSite eventSourceStack
                    addAttribute "event.destination" $ callSite stack
                    unlift $ listener event

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
