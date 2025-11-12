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


data Log :: Effect where
    Debug :: Text -> Log m ()
    Info :: Text -> Log m ()
    Warn :: Text -> Log m ()
    Err :: Text -> Log m ()


makeEffect ''Log


runLog :: (IOE :> es) => Eff (Log : es) a -> Eff es a
runLog = interpret_ $ \case
    Debug msg -> liftIO $ putTextLn $ "[DEBUG] " <> msg
    Info msg -> liftIO $ putTextLn $ "[INFO] " <> msg
    Warn msg -> liftIO $ putTextLn $ "[WARN] " <> msg
    Err msg -> liftIO $ putTextLn $ "[ERROR] " <> msg
