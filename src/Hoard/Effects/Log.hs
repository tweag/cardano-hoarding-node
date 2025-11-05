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

import Data.Text (Text)
import Effectful (Eff, Effect, IOE, liftIO, (:>))
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
    Debug msg -> liftIO $ T.putStrLn $ "[DEBUG] " <> msg
    Info msg -> liftIO $ T.putStrLn $ "[INFO] " <> msg
    Warn msg -> liftIO $ T.putStrLn $ "[WARN] " <> msg
    Err msg -> liftIO $ T.putStrLn $ "[ERROR] " <> msg
