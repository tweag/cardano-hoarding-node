module Hoard.Core (SetupConfig, component) where

import Data.Aeson (FromJSON)
import Data.Default (Default, def)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, modify)
import System.Posix.Resource (Resource (..), ResourceLimit (..), ResourceLimits (..), getResourceLimit, setResourceLimit)
import Prelude hiding (Reader, State, asks, modify)

import Hoard.CardanoNode.Config (Config (..))
import Hoard.Component (Component (..), defaultComponent)
import Hoard.Effects.HoardStateRepo (HoardStateRepo)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Events.Network (ProtocolError)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed (..), immutableTipRefreshTriggeredListener, immutableTipRefreshedListener)
import Hoard.Listeners.NetworkEventListener (protocolErrorListener)
import Hoard.Triggers (every)
import Hoard.Types.HoardState (HoardState (..))
import Hoard.Types.QuietSnake (QuietSnake (..))

import Hoard.Effects.HoardStateRepo qualified as HoardStateRepo


component
    :: ( Concurrent :> es
       , HoardStateRepo :> es
       , IOE :> es
       , Log :> es
       , NodeToClient :> es
       , Pub ImmutableTipRefreshTriggered :> es
       , Pub ImmutableTipRefreshed :> es
       , Reader Config :> es
       , Reader SetupConfig :> es
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
            refreshInterval <- asks @Config $ (.immutableTipRefreshSeconds)
            pure
                [ every refreshInterval $ publish ImmutableTipRefreshTriggered
                ]
        , setup = do
            -- Set file descriptor limits
            setFileDescriptorLimit

            -- Load immutable tip from DB and set in state
            immutableTip <- HoardStateRepo.getImmutableTip
            modify (\hoardState -> hoardState {immutableTip = immutableTip})
        , start =
            publish ImmutableTipRefreshTriggered
        }


-- | Setup configuration
data SetupConfig = SetupConfig
    { maxFileDescriptors :: Maybe Word32
    -- ^ Maximum number of open file descriptors (soft limit)
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake SetupConfig


instance Default SetupConfig where
    def = SetupConfig {maxFileDescriptors = Nothing}


-- | Default file descriptor limit (8192)
--
-- This is a reasonable default that should work on most systems
-- without requiring elevated privileges, as it's typically well
-- below the hard limit.
defaultMaxFileDescriptors :: Word32
defaultMaxFileDescriptors = 8192


-- | Set the file descriptor limit (soft limit) to the specified value.
--
-- This raises the soft limit for open file descriptors up to the specified limit,
-- which must not exceed the hard limit. If the requested limit is higher than the
-- hard limit, it will be capped at the hard limit.
setFileDescriptorLimit :: (IOE :> es, Reader SetupConfig :> es, Tracing :> es) => Eff es ()
setFileDescriptorLimit = withSpan "set_file_descriptor_limit" do
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
    when (newSoftLimit /= softLimit)
        $ liftIO
        $ setResourceLimit
            ResourceOpenFiles
            ResourceLimits {softLimit = newSoftLimit, hardLimit}
