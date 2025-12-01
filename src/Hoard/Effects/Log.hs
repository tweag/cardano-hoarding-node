module Hoard.Effects.Log
    ( -- * Effect
      Log
    , info
    , warn
    , debug
    , err

      -- * Interpreters
    , runLog
    , Config (..)
    , Severity (..)
    , defaultConfig
    , runLogWith
    ) where

import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)

import Data.Aeson (FromJSON)
import Data.Text.IO qualified as T
import Hoard.Types.JsonReadShow (JsonReadShow (..))


data Log :: Effect where
    Log :: Severity -> Text -> Log m ()


data Severity
    = DEBUG
    | INFO
    | WARN
    | ERROR
    deriving stock (Eq, Ord, Enum, Bounded, Show, Read)
    deriving (FromJSON) via (JsonReadShow Severity)


makeEffect ''Log


debug :: (Log :> es) => Text -> Eff es ()
debug = log DEBUG


info :: (Log :> es) => Text -> Eff es ()
info = log INFO


warn :: (Log :> es) => Text -> Eff es ()
warn = log WARN


err :: (Log :> es) => Text -> Eff es ()
err = log ERROR


runLog :: (IOE :> es) => Eff (Log : es) a -> Eff es a
runLog = interpret_ $ \(Log severity msg) -> liftIO $ do
    T.hPutStrLn stdout $ "[" <> (show severity) <> "] " <> msg
    hFlush stdout


data Config = Config
    { minimumSeverity :: Severity
    , output :: Handle
    }


defaultConfig :: Config
defaultConfig =
    Config
        { minimumSeverity = minBound
        , output = stdout
        }


runLogWith :: (IOE :> es) => Config -> Eff (Log : es) a -> Eff es a
runLogWith config = interpret_ $ \(Log severity msg) -> liftIO $
    when (severity >= config.minimumSeverity) $ do
        T.hPutStrLn config.output $ "[" <> (show severity) <> "] " <> msg
        hFlush stdout
