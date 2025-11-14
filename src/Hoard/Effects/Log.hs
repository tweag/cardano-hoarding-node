module Hoard.Effects.Log
    ( -- * Effect
      Log
    , info
    , warn
    , debug
    , err

      -- * Interpreters
    , runLog
    ) where

import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)

import Data.Text.IO qualified as T


data Log :: Effect where
    Debug :: Text -> Log m ()
    Info :: Text -> Log m ()
    Warn :: Text -> Log m ()
    Err :: Text -> Log m ()


makeEffect ''Log


runLog :: (IOE :> es) => Eff (Log : es) a -> Eff es a
runLog = interpret_ $ \case
    Debug msg -> log "DEBUG" msg
    Info msg -> log "INFO" msg
    Warn msg -> log "WARN" msg
    Err msg -> log "ERROR" msg
  where
    log kind msg = liftIO $ do
        T.hPutStrLn stdout $ "[" <> kind <> "] " <> msg
        hFlush stdout
