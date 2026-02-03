-- |
-- Module: Hoard.Setup
-- Description: Application setup and initialization
--
-- This module handles early application setup tasks like setting resource limits.
module Hoard.Setup
    ( setup
    ) where

import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, modify)
import System.Posix.Resource (Resource (..), ResourceLimit (..), ResourceLimits (..), getResourceLimit, setResourceLimit)
import Prelude hiding (Reader, State, asks, modify)

import Hoard.Effects.HoardStateRepo (HoardStateRepo)
import Hoard.Effects.HoardStateRepo qualified as HoardStateRepo
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Pub)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered))
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed (ImmutableTipRefreshed), immutableTipRefreshTriggeredListener, immutableTipRefreshedListener)
import Hoard.Types.Environment (Config (..))
import Hoard.Types.HoardState (HoardState (..))


-- | Default file descriptor limit (8192)
--
-- This is a reasonable default that should work on most systems
-- without requiring elevated privileges, as it's typically well
-- below the hard limit.
defaultMaxFileDescriptors :: Word32
defaultMaxFileDescriptors = 8192


-- | Perform early application setup.
--
-- This includes:
-- - Setting file descriptor limits based on configuration
-- - Any other early initialization tasks
--
-- Should be called early in the application startup, before opening many files
-- or network connections.
setup
    :: ( IOE :> es
       , Log :> es
       , Reader Config :> es
       , NodeToClient :> es
       , State HoardState :> es
       , Pub :> es
       , HoardStateRepo :> es
       )
    => Eff es ()
setup = do
    Log.info "Running application setup..."

    setFileDescriptorLimit
    immutableTip <- HoardStateRepo.getImmutableTip
    modify (\hoardState -> hoardState {immutableTip = immutableTip})

    -- listeners have not been registered yet. so we call these 2 directly rather than dispatching an event.
    immutableTipRefreshTriggeredListener ImmutableTipRefreshTriggered
    immutableTipRefreshedListener ImmutableTipRefreshed

    Log.info "Application setup complete"


-- | Set the file descriptor limit (soft limit) to the specified value.
--
-- This raises the soft limit for open file descriptors up to the specified limit,
-- which must not exceed the hard limit. If the requested limit is higher than the
-- hard limit, it will be capped at the hard limit.
setFileDescriptorLimit :: (IOE :> es, Log :> es, Reader Config :> es) => Eff es ()
setFileDescriptorLimit = do
    -- Set file descriptor limit (use default if not configured)
    configLimit <- asks (.maxFileDescriptors)
    let limit = fromMaybe defaultMaxFileDescriptors configLimit

    -- Get current limits
    ResourceLimits {softLimit, hardLimit} <- liftIO $ getResourceLimit ResourceOpenFiles

    Log.debug $ "Current file descriptor limits - soft: " <> show softLimit <> ", hard: " <> show hardLimit

    -- Convert requested limit to ResourceLimit
    let requested = ResourceLimit (fromIntegral limit)

    -- Determine the new soft limit (capped by hard limit)
    let newSoftLimit = case hardLimit of
            ResourceLimitInfinity -> requested
            ResourceLimitUnknown -> requested
            ResourceLimit hard ->
                if fromIntegral limit > hard
                    then ResourceLimit hard
                    else requested

    -- Only update if different from current
    when (newSoftLimit /= softLimit) $ do
        Log.info $ "Setting file descriptor soft limit to: " <> show newSoftLimit
        liftIO $ setResourceLimit ResourceOpenFiles ResourceLimits {softLimit = newSoftLimit, hardLimit}
        Log.info "File descriptor limit updated successfully"
