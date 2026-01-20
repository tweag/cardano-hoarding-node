module Hoard.Types.Environment
    ( ServerConfig (..)
    , LogConfig (..)
    , Severity (..)
    , Config (..)
    , Handles (..)
    , Topology (..)
    , MiniProtocolConfig (..)

      -- * peer-snapshot.json
    , PeerSnapshotFile (..)
    , LedgerPool (..)
    , BootstrapPeerIP (..)
    , BootstrapPeerDomain (..)

      -- * Misc
    , Env (..)
    , NodeSocketsConfig (..)
    , SshTunnel (..)
    , Local (..)
    , defaultLogConfig
    )
where

import Cardano.Api (NodeConfig)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Time (NominalDiffTime)
import Effectful.Concurrent.QSem (QSem)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import Ouroboros.Network.IOManager (IOManager)
import Ouroboros.Network.Mux (MiniProtocolLimits (..))

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
    , topology :: Topology
    , peerSnapshot :: PeerSnapshotFile
    , peerFailureCooldown :: NominalDiffTime
    , blockFetchQSem :: QSem
    , miniProtocolConfig :: MiniProtocolConfig
    }


data MiniProtocolConfig = MiniProtocolConfig
    { blockFetch :: MiniProtocolLimits
    , chainSync :: MiniProtocolLimits
    , txSubmission :: MiniProtocolLimits
    , keepAlive :: MiniProtocolLimits
    , peerSharing :: MiniProtocolLimits
    }


data Topology = Topology
    { peerSnapshotFile :: FilePath
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)


data PeerSnapshotFile = PeerSnapshotFile
    { bigLedgerPools :: [LedgerPool]
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)


data LedgerPool = LedgerPool
    { relays :: [Either BootstrapPeerDomain BootstrapPeerIP]
    }
    deriving stock (Eq, Generic, Show)


-- Custom FromJSON instance for LedgerPool to handle Either in relays
instance FromJSON LedgerPool where
    parseJSON = withObject "LedgerPool" $ \o -> do
        relaysArray <- o .: "relays"
        relays <- forM relaysArray $ \relayValue ->
            -- Try parsing as domain first, then as IP
            (Left <$> parseJSON @BootstrapPeerDomain relayValue)
                <|> (Right <$> parseJSON @BootstrapPeerIP relayValue)
        pure $ LedgerPool {relays}


data BootstrapPeerIP = BootstrapPeerIP
    { address :: Text
    , port :: Int
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)


data BootstrapPeerDomain = BootstrapPeerDomain
    { domain :: Text
    , port :: Int
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)


-- | Runtime handles and resources
data Handles = Handles
    { ioManager :: IOManager
    , dbPools :: DBPools
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
