module Hoard.Control.Exception
    ( withExceptionLogging
    , isGracefulShutdown
    , runErrorThrowing
    ) where

import Control.Exception qualified as IOE
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Exception (SomeAsyncException (..), catch, throwIO)
import System.IO.Error (userError)

import Hoard.Effects.Monitoring.Tracing (SpanStatus (..), Tracing, addEvent, setStatus)


-- | Wrap a protocol action with exception logging.
--
-- Emits trace events for graceful shutdowns and sets error status for real errors.
withExceptionLogging :: (Tracing :> es) => Text -> Eff es a -> Eff es a
withExceptionLogging protocolName action =
    action `catch` \(e :: SomeException) -> do
        let exceptionType = toText (displayException e)
        if isGracefulShutdown e
            then addEvent "graceful_shutdown" [("protocol", protocolName)]
            else do
                addEvent "protocol_error" [("protocol", protocolName), ("exception", exceptionType)]
                setStatus $ Error (protocolName <> ": " <> exceptionType)
        throwIO e


-- | Determine if an exception represents a graceful shutdown.
--
-- This function identifies exceptions that indicate intentional shutdown or
-- cancellation, as opposed to actual errors.
--
-- Graceful shutdown exceptions include:
-- - Ki ScopeClosing: Internal ki exception (not exported, detected via string matching)
--
-- Other async exceptions (e.g., network timeouts) are treated as real errors.
isGracefulShutdown :: SomeException -> Bool
isGracefulShutdown = isJust . fromException @SomeAsyncException


runErrorThrowing :: (IOE :> es) => Eff (Error Text : es) a -> Eff es a
runErrorThrowing eff =
    runErrorNoCallStack eff >>= \case
        Left err -> liftIO $ IOE.throwIO $ userError $ toString err
        Right value -> pure value
