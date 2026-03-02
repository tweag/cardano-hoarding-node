module Hoard.CardanoNode.Config
    ( Config (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))

import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Configuration for integrating with a Cardano node
newtype Config = Config
    { sshServerAliveIntervalSeconds :: Int
    -- ^ SSH tunnel keepalive interval
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { sshServerAliveIntervalSeconds = 60
            }
