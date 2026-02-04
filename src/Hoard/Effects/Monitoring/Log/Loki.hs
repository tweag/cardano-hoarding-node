-- | Loki integration for Log effect
--
-- Interposes on the Log effect to send messages to Loki in addition to stdout.
module Hoard.Effects.Monitoring.Log.Loki
    ( runLogLoki
    , runLogLokiFromConfig
    ) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interposeWith, localSeqUnlift)
import Effectful.Reader.Static (Reader, ask)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenTelemetry.Trace.Core qualified as OT
import OpenTelemetry.Trace.Id (Base (..), spanIdBaseEncodedText, traceIdBaseEncodedText)
import Prelude hiding (Reader, ask)

import Hoard.Effects.Log (Log (..), Message (..), Namespace (..), getNamespace, logMsg, withNamespace)
import Hoard.Effects.Monitoring.Log.Loki.Client (sendToLoki)
import Hoard.Effects.Monitoring.Log.Loki.Types (LokiLabels (..), LokiPushRequest (..), mkLokiEntry, mkLokiStream)
import Hoard.Effects.Monitoring.Tracing (Tracing, getSpanContext)
import Hoard.Types.Environment (Config (..), LokiConfig (..))


-- | Interpose on Log effect to send messages to Loki (in addition to other handlers)
--
-- Captures trace context from active OpenTelemetry spans and includes trace_id
-- and span_id in Loki logs for correlation with distributed traces.
runLogLoki :: (IOE :> es, Log :> es, Tracing :> es) => LokiConfig -> Eff es a -> Eff es a
runLogLoki config action
    | not config.enabled = action
    | otherwise = do
        manager <- liftIO $ newManager tlsManagerSettings

        interposeWith action $ \env -> \case
            LogMsg msg -> do
                -- Capture current trace context
                maybeSpanCtx <- getSpanContext
                -- Send to Loki immediately (fire-and-forget, non-blocking)
                liftIO $ sendLogToLoki manager config msg maybeSpanCtx
                -- Pass through to the next handler (e.g., stdout)
                logMsg msg
            WithNamespace ns act -> localSeqUnlift env $ \unlift ->
                -- Pass through namespace scoping
                withNamespace ns $ unlift act
            GetNamespace ->
                -- Pass through namespace retrieval
                getNamespace
  where
    sendLogToLoki manager cfg msg maybeSpanCtx = do
        now <- getPOSIXTime
        let Namespace ns = msg.namespace
            labels =
                LokiLabels
                    { namespace = if ns == "" then "default" else ns
                    , severity = show msg.severity
                    , service = cfg.serviceName
                    }
            -- Format message with trace context
            formatted = formatMessageWithTrace msg maybeSpanCtx
            entry = mkLokiEntry now formatted
            stream = mkLokiStream labels [entry]
            pushRequest = LokiPushRequest {streams = [stream]}

        sendToLoki manager cfg.endpoint pushRequest

    formatMessageWithTrace msg maybeSpanCtx =
        case maybeSpanCtx of
            Nothing -> msg.text
            Just spanCtx ->
                let traceIdHex = traceIdBaseEncodedText Base16 $ OT.traceId spanCtx
                    spanIdHex = spanIdBaseEncodedText Base16 $ OT.spanId spanCtx
                in  msg.text <> " trace_id=" <> traceIdHex <> " span_id=" <> spanIdHex


-- | Run Loki integration with config from Reader
--
-- Convenience wrapper that reads LokiConfig from the environment.
runLogLokiFromConfig
    :: (IOE :> es, Log :> es, Tracing :> es, Reader Config :> es)
    => Eff es a
    -> Eff es a
runLogLokiFromConfig action = do
    Config {loki} <- ask
    runLogLoki loki action
