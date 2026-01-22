module Hoard.Collectors.Config (Config (..)) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Data.Time (NominalDiffTime)
import Hoard.Types.QuietSnake (QuietSnake (..))


newtype Config = Config
    { peerFailureCooldownSeconds :: NominalDiffTime
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { peerFailureCooldownSeconds = 300 -- 5 minutes
            }
