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
    , runLogWriter
    ) where

import Data.Text.IO qualified as T
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Effectful.Writer.Static.Shared (Writer, tell)
import Prelude hiding (Reader, ask)

import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
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


runLog :: (IOE :> es, Reader LogConfig :> es, Chan :> es, Conc :> es) => Eff (Log : es) a -> Eff es a
runLog action = do
    config <- ask
    (inChan, outChan) <- Chan.newChan

    -- Fork worker thread that reads from channel and writes to handle
    Conc.fork_ $ forever $ do
        (severity, msg) <- Chan.readChan outChan
        liftIO $ when (severity >= config.minimumSeverity) $ do
            T.hPutStrLn config.output $ formatMessage severity msg
            hFlush stdout

    -- Interpret Log effect to write messages to channel
    interpret_ (\(Log severity msg) -> Chan.writeChan inChan (severity, msg)) action


runLogWriter :: (Writer [Text] :> es) => Eff (Log : es) a -> Eff es a
runLogWriter =
    interpret_ \(Log severity msg) ->
        tell [formatMessage severity msg]


formatMessage :: Severity -> Text -> Text
formatMessage severity msg = "[" <> show severity <> "] " <> msg
