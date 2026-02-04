-- | Types for Loki HTTP push API
--
-- Implements the Loki push API types as specified in:
-- https://grafana.com/docs/loki/latest/api/#push-log-entries-to-loki
module Hoard.Effects.Monitoring.Log.Loki.Types
    ( LokiPushRequest (..)
    , LokiStream (..)
    , LokiEntry (..)
    , LokiLabels (..)
    , mkLokiEntry
    , mkLokiStream
    ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Time.Clock.POSIX (POSIXTime)


-- | Top-level push request containing multiple streams
newtype LokiPushRequest = LokiPushRequest
    { streams :: [LokiStream]
    }
    deriving stock (Eq, Show)


instance ToJSON LokiPushRequest where
    toJSON (LokiPushRequest streams) =
        object ["streams" .= streams]


-- | A stream groups log entries with the same labels
data LokiStream = LokiStream
    { stream :: LokiLabels
    , values :: [LokiEntry]
    }
    deriving stock (Eq, Show)


instance ToJSON LokiStream where
    toJSON (LokiStream labels entries) =
        object
            [ "stream" .= labels
            , "values" .= entries
            ]


-- | Labels for a log stream (low cardinality only!)
data LokiLabels = LokiLabels
    { namespace :: Text
    , severity :: Text
    , service :: Text
    }
    deriving stock (Eq, Show, Ord)


instance ToJSON LokiLabels where
    toJSON (LokiLabels ns sev svc) =
        object
            [ "namespace" .= ns
            , "severity" .= sev
            , "service" .= svc
            ]


-- | A single log entry (timestamp + message)
-- CRITICAL: Loki expects timestamp as nanosecond string, not number
data LokiEntry = LokiEntry
    { timestampNs :: Text -- Nanoseconds since epoch as string
    , line :: Text
    }
    deriving stock (Eq, Show)


instance ToJSON LokiEntry where
    toJSON (LokiEntry ts msg) =
        -- Loki expects a 2-element array: [timestamp, message]
        Aeson.Array $ fromList [Aeson.String ts, Aeson.String msg]


-- | Create a Loki entry from POSIX time and message
mkLokiEntry :: POSIXTime -> Text -> LokiEntry
mkLokiEntry posixTime msg =
    LokiEntry
        { timestampNs = show (floor (posixTime * 1_000_000_000) :: Integer)
        , line = msg
        }


-- | Create a Loki stream from labels and entries
mkLokiStream :: LokiLabels -> [LokiEntry] -> LokiStream
mkLokiStream labels entries =
    LokiStream
        { stream = labels
        , values = entries
        }
