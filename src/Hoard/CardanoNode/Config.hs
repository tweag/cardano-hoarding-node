module Hoard.CardanoNode.Config
    ( Config (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))

import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Configuration for integrating with a Cardano node
data Config = Config
    { sshServerAliveIntervalSeconds :: Int
    -- ^ SSH tunnel keepalive interval
    , immutableTipRefreshSeconds :: Int
    -- ^ Interval between immutable tip refresh
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { sshServerAliveIntervalSeconds = 60
            , immutableTipRefreshSeconds = 30
            }
