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
import Effectful.Concurrent.STM (Concurrent)
import Effectful.Dispatch.Dynamic (interpretWith_, interpret_)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Prelude hiding (Reader, ask)

import Effectful.Concurrent.MVar (withMVar)
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


-- | Thread-safe logging to the configured handle using `hPutStrLn`.
runLog :: (Concurrent :> es, IOE :> es, Reader LogConfig :> es) => Eff (Log : es) a -> Eff es a
runLog eff = do
    config <- ask
    var <- newMVar ()
    interpretWith_ eff $ \(Log severity msg) ->
        when (severity >= config.minimumSeverity) $
            withMVar var $ \_ -> liftIO $ do
                T.hPutStrLn config.output $ "[" <> (show severity) <> "] " <> msg
                hFlush config.output
