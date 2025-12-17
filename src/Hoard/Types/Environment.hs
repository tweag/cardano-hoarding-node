module Hoard.Types.Environment
    ( ServerConfig (..)
    , Config (..)
    , Handles (..)
    , Env (..)
    )
where

import Data.Aeson (FromJSON)
import Data.Dynamic (Dynamic)
import Ouroboros.Network.IOManager (IOManager)

import Hoard.Effects.Chan (InChan)
import Hoard.Effects.Log qualified as Log
import Hoard.Types.DBConfig (DBPools (..))
import Hoard.Types.QuietSnake (QuietSnake (..))


-- | HTTP server configuration
data ServerConfig = ServerConfig
    { host :: Text
    , port :: Word16
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake ServerConfig


-- | Pure configuration data loaded from config files
data Config = Config
    { server :: ServerConfig
    , protocolConfigPath :: FilePath
    , localNodeSocketPath :: FilePath
    , logging :: Log.Config
    }


-- | Runtime handles and resources
data Handles = Handles
    { ioManager :: IOManager
    , dbPools :: DBPools
    , inChan :: InChan Dynamic
    }


-- | Application environment combining config and handles
data Env = Env
    { config :: Config
    , handles :: Handles
    }
