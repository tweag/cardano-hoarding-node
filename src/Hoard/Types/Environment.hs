module Hoard.Types.Environment
    ( ServerConfig (..)
    , Config (..)
    , Handles (..)
    , Topology (..)

      -- * Cardano protocol configuration
    , CardanoProtocolsConfig (..)
    , TxSubmissionConfig (..)

      -- * Cardano protocol handles
    , CardanoProtocolHandles (..)

      -- * Monitoring configuration
    , MonitoringConfig (..)

      -- * Cardano node integration configuration
    , CardanoNodeIntegrationConfig (..)

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
    )
where

import Cardano.Api (NodeConfig)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import Ouroboros.Network.IOManager (IOManager)

import Hoard.BlockFetch.Config qualified as BlockFetch
import Hoard.ChainSync.Config qualified as ChainSync
import Hoard.Collectors.Config qualified as Collectors
import Hoard.Effects.Log qualified as Log
import Hoard.KeepAlive.Config qualified as KeepAlive
import Hoard.PeerSharing.Config qualified as PeerSharing
import Hoard.Types.Cardano (CardanoBlock)
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
    , nodeSockets :: NodeSocketsConfig
    , logging :: Log.Config
    , protocolInfo :: ProtocolInfo CardanoBlock
    , nodeConfig :: NodeConfig
    , maxFileDescriptors :: Maybe Word32
    , topology :: Topology
    , peerSnapshot :: PeerSnapshotFile
    , collectors :: Collectors.Config
    , cardanoProtocols :: CardanoProtocolsConfig
    , monitoring :: MonitoringConfig
    , cardanoNodeIntegration :: CardanoNodeIntegrationConfig
    }


-- | Cardano protocol configuration
data CardanoProtocolsConfig = CardanoProtocolsConfig
    { peerSharing :: PeerSharing.Config
    , keepAlive :: KeepAlive.Config
    , blockFetch :: BlockFetch.Config
    , chainSync :: ChainSync.Config
    , txSubmission :: TxSubmissionConfig
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake CardanoProtocolsConfig


-- | Transaction submission configuration
data TxSubmissionConfig = TxSubmissionConfig
    { maximumIngressQueue :: Int
    -- ^ Max bytes queued in ingress queue
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake TxSubmissionConfig


-- | Monitoring configuration
data MonitoringConfig = MonitoringConfig
    { pollingIntervalSeconds :: Int
    -- ^ Interval between peer status polling
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake MonitoringConfig


-- | Configuration for integrating with a Cardano node
data CardanoNodeIntegrationConfig = CardanoNodeIntegrationConfig
    { sshServerAliveIntervalSeconds :: Int
    -- ^ SSH tunnel keepalive interval
    , immutableTipRefreshSeconds :: Int
    -- ^ Interval between immutable tip refresh
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake CardanoNodeIntegrationConfig


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
    , cardanoProtocols :: CardanoProtocolHandles
    }


newtype CardanoProtocolHandles = CardanoProtocolHandles
    { blockFetch :: BlockFetch.Handles
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
