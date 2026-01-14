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

import Data.ByteString.Char8 qualified as B8
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpretWith_, interpret_)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Effectful.Writer.Static.Shared (Writer, tell)
import GHC.Stack (SrcLoc (..))
import Prelude hiding (Reader, ask)

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


runLog :: (IOE :> es, Reader LogConfig :> es) => Eff (Log : es) a -> Eff es a
runLog action = do
    config <- ask

    -- Interpret Log effect to write messages to channel
    interpretWith_ action \(LogMsg msg) ->
        liftIO $ when (msg.severity >= config.minimumSeverity) do
            -- NOTE: We use hPutStr here with an appended newline because
            -- hPutStrLn is not atomic for ByteStrings longer than 1024 bytes.
            -- Data.Text.IO.hPutStrLn is not atomic for even short Texts.
            B8.hPutStr config.output
                . encodeUtf8
                . (<> "\n")
                . formatMessage
                $ msg
            hFlush stdout


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
