module Hoard.Control.Exception
    ( withExceptionLogging
    , isGracefulShutdown
    ) where

import Effectful (Eff)
import Effectful.Exception (SomeAsyncException (..), catch, throwIO)

import Hoard.Effects.Log qualified as Log


-- | Wrap a protocol action with exception logging.
--
-- Logs graceful shutdowns at INFO level and real errors at ERROR level.
withExceptionLogging :: (_) => Text -> Eff es a -> Eff es a
withExceptionLogging protocolName action =
    action `catch` \(e :: SomeException) -> do
        let msg = protocolName <> ": " <> toText (displayException e)
        if isGracefulShutdown e
            then Log.info $ "graceful shutdown: " <> msg
            else Log.err $ "error: " <> msg
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
