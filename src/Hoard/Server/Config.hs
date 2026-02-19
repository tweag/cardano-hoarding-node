module Hoard.Server.Config
    ( Config (..)
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))

import Hoard.Types.QuietSnake (QuietSnake (..))


-- | HTTP server configuration
data Config = Config
    { host :: Text
    , port :: Word16
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { host = "0.0.0.0"
            , port = 3000
            }
