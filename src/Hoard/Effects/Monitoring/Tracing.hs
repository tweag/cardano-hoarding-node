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
module Hoard.Effects.Monitoring.Tracing
    ( -- * Effect
      Tracing

      -- * Span Operations
    , withSpan
    , withSpanAsChild
    , withSpanLinked
    , addAttribute
    , addEvent
    , setStatus
    , getSpanContext
    , asTracer
    , OT.ToAttribute (..)
    , Attr (..)
    , ToAttributeShow (..)

      -- * Interpreters
    , runTracing
    , runTracingFromConfig
    , runTracingNoOp

      -- * Configuration
    , TracingConfig (..)

      -- * Re-exports
    , SpanStatus (..)
    , SpanContext
    ) where

import Control.Tracer (Tracer (..))
import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, interpretWith, localSeqUnlift)
import Effectful.Exception (bracket, onException)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Prelude hiding (Reader, ask)

import Data.HashMap.Strict qualified as HashMap
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import OpenTelemetry.Trace qualified as OT
import OpenTelemetry.Trace.Core qualified as OT

import Hoard.Types.QuietSnake (QuietSnake (..))

import Hoard.Effects.Monitoring.Tracing.Provider qualified as Provider


-- | Tracing configuration for OpenTelemetry
data TracingConfig = TracingConfig
    { enabled :: Bool
    -- ^ Enable tracing
    , serviceName :: Text
    -- ^ Service name for traces
    , otlpEndpoint :: Text
    -- ^ OTLP endpoint (e.g., "http://localhost:4318")
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake TracingConfig


instance Default TracingConfig where
    def =
        TracingConfig
            { enabled = False
            , serviceName = "hoard"
            , otlpEndpoint = "http://localhost:4318"
            }


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
    -- | Execute an action within a named span as a child of an explicit parent context.
    -- Useful for preserving trace hierarchy across thread boundaries
    WithSpanAsChild :: Text -> SpanContext -> m a -> Tracing m a
    -- | Execute an action within a named span with links to other span contexts
    WithSpanLinked :: Text -> [SpanContext] -> m a -> Tracing m a
    -- | Add an attribute to the current span
    AddAttribute :: (OT.ToAttribute attr) => Text -> attr -> Tracing m ()
    -- | Add an event to the current span
    AddEvent :: (OT.ToAttribute attr) => Text -> [(Text, attr)] -> Tracing m ()
    -- | Set the status of the current span
    SetStatus :: SpanStatus -> Tracing m ()
    -- | Get the current span context (for exemplars)
    GetSpanContext :: Tracing m (Maybe SpanContext)


-- | Useful to create a heterogeneous list of attribute values. These two are equivalent:
--
-- @
-- addEvent "foo" [OT.toAttribute 1, OT.toAttribute "foo"]
-- addEvent "foo" [Attr 1, Attr "foo"]
-- @
data Attr where
    Attr :: (OT.ToAttribute a) => a -> Attr


instance OT.ToAttribute Attr where
    toAttribute (Attr a) = OT.toAttribute a


newtype ToAttributeShow a = ToAttributeShow
    { getToAttributeShow :: a
    }


instance (Show a) => OT.ToPrimitiveAttribute (ToAttributeShow a) where
    toPrimitiveAttribute = OT.TextAttribute . show . getToAttributeShow


instance (Show a) => OT.ToAttribute (ToAttributeShow a)


makeEffect ''Tracing


-- | Create a Tracer that emits protocol messages as trace events.
--
-- Useful for integrating ouroboros-network protocol tracers with OpenTelemetry.
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
    | otherwise =
        bracket
            (liftIO $ Provider.initTracingState serviceName otlpEndpoint)
            (\tracingState -> liftIO $ Provider.shutdownTracingState tracingState)
            $ \tracingState -> interpretWith action $ \env -> \case
                WithSpan spanName innerAction -> localSeqUnlift env $ \unlift -> do
                    currentCtx <- liftIO ThreadLocal.getContext
                    newSpan <- liftIO $ OT.createSpan tracingState.tracer currentCtx spanName OT.defaultSpanArguments
                    let newCtx = Context.insertSpan newSpan currentCtx
                    oldCtx <- liftIO $ ThreadLocal.attachContext newCtx
                    innerResult <-
                        unlift innerAction `onException` do
                            liftIO $ OT.setStatus newSpan (OT.Error "Exception occurred")
                    -- Restore the old context
                    liftIO $ void $ case oldCtx of
                        Just ctx -> ThreadLocal.attachContext ctx
                        Nothing -> ThreadLocal.detachContext
                    liftIO $ OT.endSpan newSpan Nothing
                    pure innerResult
                WithSpanAsChild spanName parentSpanContext innerAction -> localSeqUnlift env $ \unlift -> do
                    -- wrapSpanContext creates a non-recording span from the SpanContext,
                    -- allowing child span creation across thread boundaries
                    let parentSpan = OT.wrapSpanContext parentSpanContext
                    let parentCtx = Context.insertSpan parentSpan Context.empty
                    newSpan <- liftIO $ OT.createSpan tracingState.tracer parentCtx spanName OT.defaultSpanArguments
                    let newCtx = Context.insertSpan newSpan parentCtx
                    oldCtx <- liftIO $ ThreadLocal.attachContext newCtx
                    innerResult <-
                        unlift innerAction `onException` do
                            liftIO $ OT.setStatus newSpan (OT.Error "Exception occurred")

                    -- Restore the old context
                    liftIO $ void $ case oldCtx of
                        Just ctx -> ThreadLocal.attachContext ctx
                        Nothing -> ThreadLocal.detachContext
                    liftIO $ OT.endSpan newSpan Nothing
                    pure innerResult
                WithSpanLinked spanName linkedContexts innerAction -> localSeqUnlift env $ \unlift -> do
                    currentCtx <- liftIO ThreadLocal.getContext
                    let spanArgs =
                            OT.defaultSpanArguments
                                { OT.links = map (\ctx -> OT.NewLink ctx mempty) linkedContexts
                                }
                    newSpan <- liftIO $ OT.createSpan tracingState.tracer currentCtx spanName spanArgs
                    let newCtx = Context.insertSpan newSpan currentCtx
                    oldCtx <- liftIO $ ThreadLocal.attachContext newCtx
                    innerResult <-
                        unlift innerAction `onException` do
                            liftIO $ OT.setStatus newSpan (OT.Error "Exception occurred")

                    -- Restore the old context
                    liftIO $ void $ case oldCtx of
                        Just ctx -> ThreadLocal.attachContext ctx
                        Nothing -> ThreadLocal.detachContext
                    liftIO $ OT.endSpan newSpan Nothing
                    pure innerResult
                AddAttribute key value -> do
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
                            let attrMap = HashMap.fromList $ map (\(k, v) -> (k, OT.toAttribute v)) attributes
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


-- | Run the Tracing effect with config from Reader
--
-- Convenience wrapper that reads TracingConfig from the Reader effect.
runTracingFromConfig
    :: (IOE :> es, Reader TracingConfig :> es)
    => Eff (Tracing : es) a
    -> Eff es a
runTracingFromConfig action = do
    TracingConfig {enabled, serviceName, otlpEndpoint} <- ask
    runTracing enabled serviceName otlpEndpoint action


-- | No-op interpreter that discards all tracing operations
runTracingNoOp :: Eff (Tracing : es) a -> Eff es a
runTracingNoOp = interpret $ \env -> \case
    WithSpan _ act -> localSeqUnlift env $ \unlift -> unlift act
    WithSpanAsChild _ _ act -> localSeqUnlift env $ \unlift -> unlift act
    WithSpanLinked _ _ act -> localSeqUnlift env $ \unlift -> unlift act
    AddAttribute _ _ -> pure ()
    AddEvent _ _ -> pure ()
    SetStatus _ -> pure ()
    GetSpanContext -> pure Nothing
