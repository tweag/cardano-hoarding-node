module Hoard.Effects.Log
    ( -- * Effect
      Log
    , Message (..)
    , Severity (..)
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

import GHC.Stack (SrcLoc (..))
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Types.Environment (LogConfig (..), Severity (..))


data Log :: Effect where
    LogMsg :: Message -> Log m ()


data Message = Message
    { text :: Text
    , severity :: Severity
    , stack :: CallStack
    }


makeEffect ''Log


log :: (HasCallStack, Log :> es) => Severity -> Text -> Eff es ()
log severity text =
    withFrozenCallStack $
        logMsg
            Message
                { stack = callStack
                , severity
                , text
                }


debug :: (HasCallStack, Log :> es) => Text -> Eff es ()
debug = withFrozenCallStack $ log DEBUG


info :: (HasCallStack, Log :> es) => Text -> Eff es ()
info = withFrozenCallStack $ log INFO


warn :: (HasCallStack, Log :> es) => Text -> Eff es ()
warn = withFrozenCallStack $ log WARN


err :: (HasCallStack, Log :> es) => Text -> Eff es ()
err = withFrozenCallStack $ log ERROR


-- | Consumes `Log` effects, and discards the logged messages
runLogNoOp :: Eff (Log : es) a -> Eff es a
runLogNoOp = interpret_ $ \(LogMsg _) -> pure ()


runLog :: (IOE :> es, Reader LogConfig :> es, Chan :> es, Conc :> es) => Eff (Log : es) a -> Eff es a
runLog action = do
    config <- ask
    (inChan, outChan) <- Chan.newChan

    -- Fork worker thread that reads from channel and writes to handle
    Conc.fork_ $ forever $ do
        msg <- Chan.readChan outChan
        liftIO $ T.hPutStrLn config.output $ formatMessage msg
        liftIO $ hFlush stdout

    -- Interpret Log effect to write messages to channel
    interpret_ (\(LogMsg msg) -> Chan.writeChan inChan msg) action


runLogWriter :: (Writer [Message] :> es) => Eff (Log : es) a -> Eff es a
runLogWriter =
    interpret_ \(LogMsg msg) ->
        tell [msg]


formatMessage :: Message -> Text
formatMessage msg =
    mconcat
        [ square $ show msg.severity
        , " "
        , square $ showSourceLoc msg.stack
        , " "
        , msg.text
        ]


showSourceLoc :: CallStack -> Text
showSourceLoc cs = showCallStack
  where
    showCallStack = case getCallStack cs of
        [] -> "<unknown loc>"
        [(name, loc)] -> showLoc name loc
        (_, loc) : (callerName, _) : _ -> showLoc callerName loc
    showLoc name src =
        mconcat
            [ toText src.srcLocModule
            , "."
            , toText name
            , "#"
            , show src.srcLocStartLine
            ]


square :: Text -> Text
square s = "[" <> s <> "]"
