module Hoard.Types.Environment
    ( ServerConfig (..)
    , LogConfig (..)
    , Severity (..)
    , Config (..)
    , Handles (..)
    , Env (..)
    , NodeSocketsConfig (..)
    , SshTunnel (..)
    , Local (..)
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
    , nodeSockets :: NodeSocketsConfig
    , logging :: LogConfig
    , protocolInfo :: ProtocolInfo CardanoBlock
    , nodeConfig :: NodeConfig
    , maxFileDescriptors :: Maybe Word32
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


data NodeSocketsConfig
    = SshTunnel SshTunnel
    | Local Local
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake NodeSocketsConfig


data SshTunnel = MakeSshTunnel
    { nodeToClientSocket :: FilePath
    , tracerSocket :: FilePath
    , user :: Text
    , remoteHost :: Text
    , sshKey :: Maybe FilePath
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake SshTunnel


data Local = MakeLocal
    { nodeToClientSocket :: FilePath
    , tracerSocket :: FilePath
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Local
