module Hoard.Core (component) where

import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, modify)
import System.Posix.Resource (Resource (..), ResourceLimit (..), ResourceLimits (..), getResourceLimit, setResourceLimit)
import Prelude hiding (Reader, State, asks, modify)

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Effects.HoardStateRepo (HoardStateRepo)
import Hoard.Effects.Monitoring.Tracing (Tracing, addEvent, withSpan)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Events.Network (ProtocolError)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener
    ( ImmutableTipRefreshed (..)
    , immutableTipRefreshTriggeredListener
    , immutableTipRefreshedListener
    )
import Hoard.Listeners.NetworkEventListener (protocolErrorListener)
import Hoard.Triggers (every)
import Hoard.Types.Environment (CardanoNodeIntegrationConfig (..), Config (..))
import Hoard.Types.HoardState (HoardState (..))

import Hoard.Effects.HoardStateRepo qualified as HoardStateRepo


component
    :: ( Concurrent :> es
       , HoardStateRepo :> es
       , IOE :> es
       , NodeToClient :> es
       , Pub ImmutableTipRefreshTriggered :> es
       , Pub ImmutableTipRefreshed :> es
       , Reader Config :> es
       , State HoardState :> es
       , Sub ImmutableTipRefreshTriggered :> es
       , Sub ImmutableTipRefreshed :> es
       , Sub ProtocolError :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "Core"
        , listeners =
            pure
                [ listen protocolErrorListener
                , listen immutableTipRefreshTriggeredListener
                , listen immutableTipRefreshedListener
                ]
        , triggers = do
            refreshInterval <- asks $ (.cardanoNodeIntegration.immutableTipRefreshSeconds)
            pure
                [ every refreshInterval $ publish ImmutableTipRefreshTriggered
                ]
        , setup = do
            -- Set file descriptor limits
            setFileDescriptorLimit

            -- Load immutable tip from DB and set in state
            immutableTip <- HoardStateRepo.getImmutableTip
            modify (\hoardState -> hoardState {immutableTip = immutableTip})
        , start = do
            -- Trigger initial immutable tip refresh now that listeners are registered
            publish ImmutableTipRefreshTriggered
            addEvent "initial_tip_refresh_triggered" []
        }


-- | Set the file descriptor limit (soft limit) to the specified value.
--
-- This raises the soft limit for open file descriptors up to the specified limit,
-- which must not exceed the hard limit. If the requested limit is higher than the
-- hard limit, it will be capped at the hard limit.
setFileDescriptorLimit :: (IOE :> es, Reader Config :> es, Tracing :> es) => Eff es ()
setFileDescriptorLimit = withSpan "set_file_descriptor_limit" $ do
    -- Set file descriptor limit (use default if not configured)
    configLimit <- asks (.maxFileDescriptors)
    let limit = fromMaybe defaultMaxFileDescriptors configLimit

    -- Get current limits
    ResourceLimits {softLimit, hardLimit} <- liftIO $ getResourceLimit ResourceOpenFiles

    -- Convert requested limit to ResourceLimit
    let requested = ResourceLimit (fromIntegral limit)

    -- Determine the new soft limit (capped by hard limit)
    let newSoftLimit = case hardLimit of
            ResourceLimitInfinity -> requested
            ResourceLimitUnknown -> requested
            ResourceLimit hard ->
                if fromIntegral limit > hard then
                    ResourceLimit hard
                else
                    requested

    -- Only update if different from current
    when (newSoftLimit /= softLimit) $ do
        liftIO $ setResourceLimit ResourceOpenFiles ResourceLimits {softLimit = newSoftLimit, hardLimit}
        addEvent "file_descriptor_limit_updated" [("new_soft_limit", show newSoftLimit), ("old_soft_limit", show softLimit)]


-- | Default file descriptor limit (8192)
--
-- This is a reasonable default that should work on most systems
-- without requiring elevated privileges, as it's typically well
-- below the hard limit.
defaultMaxFileDescriptors :: Word32
defaultMaxFileDescriptors = 8192
