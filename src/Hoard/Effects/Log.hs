-- | Log effect with hierarchical namespace support.
--
-- Provides structured logging with composable namespaces for better organization.
--
-- == Basic Usage
--
-- @
-- myComponent :: (Log :> es) => Eff es ()
-- myComponent = do
--     info "Starting component"
--     -- Logs: [INFO] [MyModule.myComponent#42] Starting component
-- @
--
-- == Using Namespaces
--
-- @
-- processData :: (Log :> es) => Eff es ()
-- processData = withNamespace "processor" $ do
--     info "Processing started"
--     -- Logs: [INFO] [processor] [MyModule.processData#10] Processing started
--
--     withNamespace "validation" $ do
--         info "Validating input"
--         -- Logs: [INFO] [processor.validation] [MyModule.processData#13] Validating input
-- @
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
    , withNamespace
    , asTracer

      -- * Interpreters
    , runLog
    , runLogNoOp
    , runLogWriter
    ) where

import Control.Tracer (Tracer (..))
import Data.ByteString.Char8 qualified as B8
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (localSeqUnlift, reinterpret, reinterpretWith)
import Effectful.Reader.Static (Reader, ask, local, runReader)
import Effectful.TH (makeEffect)
import Effectful.Writer.Static.Shared (Writer, tell)
import GHC.Stack (SrcLoc (..))
import Prelude hiding (Reader, ask, local, runReader)

import Hoard.Types.Environment (LogConfig (..), Severity (..))


data Log :: Effect where
    LogMsg :: Message -> Log m ()
    WithNamespace :: Namespace -> m a -> Log m a
    GetNamespace :: Log m Namespace


data Message = Message
    { namespace :: Namespace
    , text :: Text
    , severity :: Severity
    , stack :: CallStack
    }


newtype Namespace = Namespace Text
    deriving stock (Show, Eq)
    deriving newtype (IsString)


instance Semigroup Namespace where
    Namespace "" <> Namespace b = Namespace b
    Namespace a <> Namespace "" = Namespace a
    Namespace a <> Namespace b = Namespace (a <> "." <> b)


makeEffect ''Log


log :: (HasCallStack, Log :> es) => Severity -> Text -> Eff es ()
log severity text = do
    namespace <- getNamespace
    withFrozenCallStack $
        logMsg
            Message
                { stack = callStack
                , namespace
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


asTracer :: (Log :> es) => (forall x. Eff es x -> m x) -> Severity -> Tracer m String
asTracer unlift severity =
    Tracer $ \msg -> unlift $ log severity $ toText msg


-- | Consumes `Log` effects, and discards the logged messages
runLogNoOp :: Eff (Log : es) a -> Eff es a
runLogNoOp = reinterpret (runReader (Namespace "")) $ \env -> \case
    LogMsg _ -> pure ()
    WithNamespace ns act -> localSeqUnlift env $ \unlift ->
        local (<> ns) $ unlift act
    GetNamespace -> ask


runLog :: (IOE :> es, Reader LogConfig :> es) => Eff (Log : es) a -> Eff es a
runLog action = do
    config <- ask

    reinterpretWith (runReader (Namespace "")) action \env -> \case
        LogMsg msg ->
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
        WithNamespace ns act -> localSeqUnlift env $ \unlift ->
            local (<> ns) $ unlift act
        GetNamespace -> ask


runLogWriter :: (Writer [Message] :> es) => Eff (Log : es) a -> Eff es a
runLogWriter = reinterpret (runReader (Namespace "")) $ \env -> \case
    LogMsg msg -> tell [msg]
    WithNamespace ns act -> localSeqUnlift env $ \unlift ->
        local (<> ns) $ unlift act
    GetNamespace -> ask


formatMessage :: Message -> Text
formatMessage msg =
    mconcat
        [ square $ show msg.severity
        , " "
        , showNamespace msg.namespace
        , square $ showSourceLoc msg.stack
        , " "
        , msg.text
        ]
  where
    showNamespace (Namespace "") = ""
    showNamespace (Namespace ns) = square ns <> " "


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
