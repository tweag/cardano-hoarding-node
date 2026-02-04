-- | Tracing effect for distributed tracing with OpenTelemetry.
--
-- Provides operations for creating spans, adding attributes, and propagating trace context.
--
-- == Basic Usage
--
-- @
-- myHandler :: (Tracing :> es) => Eff es ()
-- myHandler = do
--     withSpan "api.create_user" $ do
--         addAttribute "user.id" "123"
--         -- ... do work ...
--         setStatus Ok
-- @
--
-- == Database Instrumentation
--
-- @
-- runQuery :: (Tracing :> es, Metrics :> es, Clock :> es) => Eff es Result
-- runQuery = do
--     withDBSpan "users.select" $ do
--         -- Query execution with automatic timing and tracing
--         executeQuery
-- @
module Hoard.Effects.Monitoring.Tracing
    ( -- * Effect
      Tracing

      -- * Span Operations
    , withSpan
    , addAttribute
    , addEvent
    , setStatus
    , getSpanContext
    , asTracer

      -- * Interpreters
    , runTracing
    , runTracingFromConfig
    , runTracingNoOp

      -- * Re-exports
    , SpanStatus (..)
    , SpanContext
    ) where

import Control.Tracer (Tracer (..))
import Data.HashMap.Strict qualified as HashMap
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, interpretWith, localSeqUnlift)
import Effectful.Exception (onException)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import OpenTelemetry.Trace qualified as OT
import OpenTelemetry.Trace.Core qualified as OT
import Prelude hiding (Reader, ask)

import Hoard.Effects.Monitoring.Tracing.Provider qualified as Provider
import Hoard.Types.Environment (Config (..), TracingConfig (..))


-- | Span status for indicating success or failure
data SpanStatus
    = Ok
    | Error Text
    deriving stock (Eq, Show)


-- | Opaque span context for correlating traces with metrics
type SpanContext = OT.SpanContext


-- | Tracing effect for distributed tracing
data Tracing :: Effect where
    -- | Execute an action within a named span (bracket-style, automatic cleanup)
    WithSpan :: Text -> m a -> Tracing m a
    -- | Add an attribute to the current span
    AddAttribute :: Text -> Text -> Tracing m ()
    -- | Add an event to the current span
    AddEvent :: Text -> [(Text, Text)] -> Tracing m ()
    -- | Set the status of the current span
    SetStatus :: SpanStatus -> Tracing m ()
    -- | Get the current span context (for exemplars)
    GetSpanContext :: Tracing m (Maybe SpanContext)


makeEffect ''Tracing


-- | Create a Tracer that emits protocol messages as trace events
--
-- This is useful for integrating ouroboros-network protocol tracers with OpenTelemetry.
-- Protocol messages are emitted as trace events within the current span.
asTracer :: (Tracing :> es) => (forall x. Eff es x -> m x) -> Text -> Tracer m String
asTracer unlift eventName =
    Tracer $ \msg -> unlift $ addEvent eventName [("message", toText msg)]


-- | Run the Tracing effect with OpenTelemetry
--
-- Initializes the tracer provider and manages span lifecycle.
runTracing
    :: (IOE :> es)
    => Bool
    -- ^ Tracing enabled flag
    -> Text
    -- ^ Service name
    -> Text
    -- ^ OTLP endpoint
    -> Eff (Tracing : es) a
    -> Eff es a
runTracing enabled serviceName otlpEndpoint action
    | not enabled = runTracingNoOp action
    | otherwise = do
        -- Initialize tracing state
        tracingState <- liftIO $ Provider.initTracingState serviceName otlpEndpoint

        result <- interpretWith action $ \env -> \case
            WithSpan spanName innerAction -> localSeqUnlift env $ \unlift -> do
                -- Get current context (contains parent span if any)
                currentCtx <- liftIO ThreadLocal.getContext

                -- Create a new span with the current context as parent
                newSpan <- liftIO $ OT.createSpan tracingState.tracer currentCtx spanName OT.defaultSpanArguments

                -- Insert the new span into context
                let newCtx = Context.insertSpan newSpan currentCtx

                -- Attach the new context
                oldCtx <- liftIO $ ThreadLocal.attachContext newCtx

                -- Run the inner action with exception handling
                innerResult <-
                    unlift innerAction `onException` do
                        -- On exception, mark span as error
                        liftIO $ OT.setStatus newSpan (OT.Error "Exception occurred")

                -- Restore the old context
                liftIO $ void $ case oldCtx of
                    Just ctx -> ThreadLocal.attachContext ctx
                    Nothing -> ThreadLocal.detachContext

                -- End the span
                liftIO $ OT.endSpan newSpan Nothing

                pure innerResult
            AddAttribute key value -> do
                -- Get current span from thread-local context
                currentCtx <- liftIO ThreadLocal.getContext
                case Context.lookupSpan currentCtx of
                    Just currentSpan ->
                        liftIO $ OT.addAttribute currentSpan key (OT.toAttribute value)
                    Nothing ->
                        -- No active span, ignore
                        pure ()
            AddEvent eventName attributes -> do
                currentCtx <- liftIO ThreadLocal.getContext
                case Context.lookupSpan currentCtx of
                    Just currentSpan -> do
                        -- Convert list of attributes to AttributeMap (HashMap Text Attribute)
                        let attrMap = HashMap.fromList $ map (\(k, v) -> (k, OT.toAttribute v)) attributes
                        -- Create NewEvent with name, attributes, and no explicit timestamp
                        let event = OT.NewEvent eventName attrMap Nothing
                        liftIO $ OT.addEvent currentSpan event
                    Nothing ->
                        -- No active span, ignore
                        pure ()
            SetStatus status -> do
                currentCtx <- liftIO ThreadLocal.getContext
                case Context.lookupSpan currentCtx of
                    Just currentSpan ->
                        liftIO $ case status of
                            Ok -> OT.setStatus currentSpan OT.Ok
                            Error msg -> OT.setStatus currentSpan (OT.Error msg)
                    Nothing ->
                        -- No active span, ignore
                        pure ()
            GetSpanContext -> do
                currentCtx <- liftIO ThreadLocal.getContext
                case Context.lookupSpan currentCtx of
                    Just currentSpan -> do
                        spanCtx <- liftIO $ OT.getSpanContext currentSpan
                        pure $ Just spanCtx
                    Nothing -> pure Nothing

        -- Cleanup
        liftIO $ Provider.shutdownTracingState tracingState

        pure result


-- | Run the Tracing effect with config from Reader
--
-- Convenience wrapper that reads TracingConfig from the environment.
runTracingFromConfig
    :: (IOE :> es, Reader Config :> es)
    => Eff (Tracing : es) a
    -> Eff es a
runTracingFromConfig action = do
    Config {tracing = TracingConfig {enabled, serviceName, otlpEndpoint}} <- ask
    runTracing enabled serviceName otlpEndpoint action


-- | No-op interpreter that discards all tracing operations
runTracingNoOp :: Eff (Tracing : es) a -> Eff es a
runTracingNoOp = interpret $ \env -> \case
    WithSpan _ act -> localSeqUnlift env $ \unlift -> unlift act
    AddAttribute _ _ -> pure ()
    AddEvent _ _ -> pure ()
    SetStatus _ -> pure ()
    GetSpanContext -> pure Nothing
