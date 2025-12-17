module Hoard.Types.Environment
    ( ServerConfig (..)
    , LogConfig (..)
    , Severity (..)
    , Config (..)
    , Handles (..)
    , Env (..)
    , defaultLogConfig
    )
where

import Cardano.Api (NodeConfig)
import Data.Aeson (FromJSON)
import Data.Dynamic (Dynamic)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import Ouroboros.Network.IOManager (IOManager)

import Hoard.Effects.Chan (InChan)
import Hoard.Types.Cardano (CardanoBlock)
import Hoard.Types.DBConfig (DBPools (..))
import Hoard.Types.JsonReadShow (JsonReadShow (..))
import Hoard.Types.QuietSnake (QuietSnake (..))


-- | HTTP server configuration
data ServerConfig = ServerConfig
    { host :: Text
    , port :: Word16
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake ServerConfig


data LogConfig = LogConfig
    { minimumSeverity :: Severity
    , output :: Handle
    }


data Severity
    = DEBUG
    | INFO
    | WARN
    | ERROR
    deriving stock (Eq, Ord, Enum, Bounded, Show, Read)
    deriving (FromJSON) via (JsonReadShow Severity)


defaultLogConfig :: LogConfig
defaultLogConfig =
    LogConfig
        { minimumSeverity = minBound
        , output = stdout
        }


-- | Pure configuration data loaded from config files
data Config = Config
    { server :: ServerConfig
    , localNodeSocketPath :: FilePath
    , logging :: LogConfig
    , protocolInfo :: ProtocolInfo CardanoBlock
    , nodeConfig :: NodeConfig
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
