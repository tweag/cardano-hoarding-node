module Hoard.Core (SetupConfig, component) where

import Data.Aeson (FromJSON)
import Data.Default (Default, def)
import Effectful (IOE)
import Effectful.Reader.Static (Reader, asks)
import System.Posix.Resource (Resource (..), ResourceLimit (..), ResourceLimits (..), getResourceLimit, setResourceLimit)

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Effects.Publishing (Sub, listen)
import Hoard.Events.Network (ProtocolError)
import Hoard.Listeners.NetworkEventListener (protocolErrorListener)
import Hoard.Types.QuietSnake (QuietSnake (..))


component
    :: ( IOE :> es
       , Log :> es
       , Reader SetupConfig :> es
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
                ]
        , setup = do
            -- Set file descriptor limits
            setFileDescriptorLimit
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
