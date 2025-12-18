module Hoard.Effects.Log
    ( -- * Effect
      Log
    , log
    , info
    , warn
    , debug
    , err

      -- * Interpreters
    , runLog
    , runLogNoOp
    ) where

import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)

import Data.Text.IO qualified as T
import Hoard.Types.Environment (LogConfig (..), Severity (..))


data Log :: Effect where
    Log :: Severity -> Text -> Log m ()


makeEffect ''Log


debug :: (Log :> es) => Text -> Eff es ()
debug = log DEBUG


info :: (Log :> es) => Text -> Eff es ()
info = log INFO


warn :: (Log :> es) => Text -> Eff es ()
warn = log WARN


err :: (Log :> es) => Text -> Eff es ()
err = log ERROR


-- | Consumes `Log` effects, and discards the logged messages
runLogNoOp :: Eff (Log : es) a -> Eff es a
runLogNoOp = interpret_ $ \(Log _ _) -> pure ()


runLog :: (IOE :> es) => LogConfig -> Eff (Log : es) a -> Eff es a
runLog config = interpret_ $ \(Log severity msg) -> liftIO $
    when (severity >= config.minimumSeverity) $ do
        T.hPutStrLn config.output $ "[" <> (show severity) <> "] " <> msg
        hFlush stdout
