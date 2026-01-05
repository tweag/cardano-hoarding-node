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

import Data.Text.IO qualified as T
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Prelude hiding (Reader, ask)

import Hoard.Types.Environment (LogConfig (..), Severity (..))


data Log :: Effect where
    Log :: Severity -> Text -> Log m ()


makeEffect ''Log


debug :: (_) => Text -> Eff es ()
debug = log DEBUG


info :: (_) => Text -> Eff es ()
info = log INFO


warn :: (_) => Text -> Eff es ()
warn = log WARN


err :: (_) => Text -> Eff es ()
err = log ERROR


-- | Consumes `Log` effects, and discards the logged messages
runLogNoOp :: Eff (Log : es) a -> Eff es a
runLogNoOp = interpret_ $ \(Log _ _) -> pure ()


runLog :: (IOE :> es, Reader LogConfig :> es) => Eff (Log : es) a -> Eff es a
runLog = interpret_ $ \(Log severity msg) -> do
    config <- ask
    liftIO $ when (severity >= config.minimumSeverity) $ do
        T.hPutStrLn config.output $ "[" <> (show severity) <> "] " <> msg
        hFlush stdout
