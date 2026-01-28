-- | Metrics effect for Prometheus metrics collection.
--
-- Provides operations for tracking application metrics.
--
-- == Basic Usage
--
-- @
-- myComponent :: (Metrics :> es) => Eff es ()
-- myComponent = do
--     gaugeSet "hoard_connected_peers" 5.0
--     counterInc "hoard_blocks_received_total"
-- @
--
-- == Available Metric Types
--
-- * Gauges: Point-in-time values that can go up or down
-- * Counters: Monotonically increasing values
-- * Histograms: Distributions with buckets for measuring durations
module Hoard.Effects.Metrics
    ( -- * Effect
      Metrics

      -- * Gauge Operations
    , gaugeSet
    , gaugeInc
    , gaugeDec

      -- * Counter Operations
    , counterInc
    , counterAdd

      -- * Histogram Operations
    , histogramObserve

      -- * Export Operations
    , exportMetrics

      -- * Interpreters
    , runMetrics
    , runMetricsNoOp
    , withHistogramTiming
    ) where

import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpretWith_, interpret_)
import Effectful.TH (makeEffect)
import Prometheus qualified as Prom
import Prometheus.Metric.GHC qualified as GHC
import Prelude hiding (Reader, ask)

import Data.Time.Clock (diffUTCTime)
import Hoard.Effects.Clock (Clock, currentTime)
import Hoard.Effects.Metrics.Registry qualified as Registry


-- | Metrics effect for tracking application metrics
data Metrics :: Effect where
    GaugeSet :: Text -> Double -> Metrics m ()
    GaugeInc :: Text -> Metrics m ()
    GaugeDec :: Text -> Metrics m ()
    CounterInc :: Text -> Metrics m ()
    CounterAdd :: Text -> Double -> Metrics m ()
    HistogramObserve :: Text -> Double -> Metrics m ()
    ExportMetrics :: Metrics m Text


makeEffect ''Metrics


-- | Run the Metrics effect with prometheus-client
--
-- Initializes the metric registry and registers GHC metrics automatically.
runMetrics
    :: (IOE :> es)
    => Eff (Metrics : es) a
    -> Eff es a
runMetrics action = do
    -- Initialize metrics registry and register GHC metrics
    handles <- liftIO Registry.initMetricHandles
    void $ liftIO $ Prom.register GHC.ghcMetrics
    interpretWith_ action \case
        GaugeSet name value -> liftIO $ Registry.setGauge handles name value
        GaugeInc name -> liftIO $ Registry.incGauge handles name
        GaugeDec name -> liftIO $ Registry.decGauge handles name
        CounterInc name -> liftIO $ Registry.incCounter handles name
        CounterAdd name value -> liftIO $ Registry.addCounter handles name value
        HistogramObserve name value -> liftIO $ Registry.observeHistogram handles name value
        ExportMetrics -> liftIO $ decodeUtf8 <$> Prom.exportMetricsAsText


-- | No-op interpreter that discards all metrics operations
runMetricsNoOp :: Eff (Metrics : es) a -> Eff es a
runMetricsNoOp = interpret_ \case
    GaugeSet _ _ -> pure ()
    GaugeInc _ -> pure ()
    GaugeDec _ -> pure ()
    CounterInc _ -> pure ()
    CounterAdd _ _ -> pure ()
    HistogramObserve _ _ -> pure ()
    ExportMetrics -> pure ""


-- | Time an action and record its duration to a histogram metric
withHistogramTiming
    :: (Metrics :> es, Clock :> es)
    => Text
    -- ^ Histogram metric name
    -> Eff es a
    -- ^ Action to time
    -> Eff es a
withHistogramTiming metricName action = do
    start <- currentTime
    result <- action
    end <- currentTime
    let duration = realToFrac $ diffUTCTime end start
    histogramObserve metricName duration
    pure result
