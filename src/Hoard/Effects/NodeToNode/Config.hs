module Hoard.Effects.NodeToNode.Config (Config (..)) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Data.Time (NominalDiffTime)

import Hoard.Types.QuietSnake (QuietSnake (..))


data Config = Config
    { connectionTimeoutSeconds :: NominalDiffTime
    -- ^ Timeout for the TCP connection handshake. If the socket doesn't
    -- connect within this duration, the connection attempt fails. This does
    -- NOT affect already-established connections.
    }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via (QuietSnake Config)


instance Default Config where
    def =
        Config
            { connectionTimeoutSeconds = 10
            }
