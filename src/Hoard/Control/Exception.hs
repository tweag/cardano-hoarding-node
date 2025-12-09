module Hoard.Control.Exception
    ( withExceptionLogging
    ) where

import Effectful (Eff, (:>))
import Effectful.Exception (AsyncException (..), catch, throwIO)

import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log


-- | Wrap a protocol action with exception logging to debug cancellations.
withExceptionLogging :: (Log :> es) => Text -> Eff es a -> Eff es a
withExceptionLogging protocolName action =
    action `catch` \(e :: SomeException) -> do
        case fromException e of
            Just ThreadKilled -> do
                log "killed: ThreadKilled"
                log "This is likely due to the Ki scope cleanup or connection closure"
            Just UserInterrupt -> do
                log "interrupted: UserInterrupt"
            Just (asyncEx :: AsyncException) -> do
                log $ "async exception: " <> show asyncEx
            Nothing -> do
                log $ "terminated with exception: " <> show e
        -- Re-throw the exception after logging
        throwIO e
  where
    log msg = Log.err $ protocolName <> ": " <> msg
