module Hoard.BlockFetch
    ( -- * Main
      component

      -- * Config
    , Config (..)
    , runConfig
    ) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static (Reader, ask, runReader)
import Prelude hiding (Reader, ask, runReader)

import Hoard.BlockFetch.Events (BlockBatchCompleted, BlockFetchFailed, BlockFetchStarted, BlockReceived)
import Hoard.Component (Component (..), defaultComponent)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.ConfigPath (ConfigPath, loadYaml)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Quota (Quota)
import Hoard.Effects.Verifier (Verifier)
import Hoard.Types.QuietSnake (QuietSnake (..))

import Hoard.BlockFetch.Listeners qualified as Listeners
import Hoard.Effects.Publishing qualified as Sub


component
    :: ( BlockRepo :> es
       , Metrics :> es
       , Quota (ID Peer, Int64) :> es
       , Sub BlockBatchCompleted :> es
       , Sub BlockFetchFailed :> es
       , Sub BlockFetchStarted :> es
       , Sub BlockReceived :> es
       , Tracing :> es
       , Verifier :> es
       )
    => Component es
component =
    defaultComponent
        { name = "BlockFetch"
        , listeners =
            pure
                [ Sub.listen Listeners.blockFetchStarted
                , Sub.listen Listeners.blockReceived
                , Sub.listen Listeners.blockFetchFailed
                , Sub.listen Listeners.blockBatchCompleted
                ]
        }


data Config = Config
    { batchSize :: Int
    -- ^ Number of block fetch requests to batch
    , batchTimeoutMicroseconds :: Int
    -- ^ Timeout for batching block fetch requests
    , maximumIngressQueue :: Int
    -- ^ Max bytes queued in ingress queue
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { batchSize = 10
            , batchTimeoutMicroseconds = 10_000_000 -- 10 seconds
            , maximumIngressQueue = 393216 -- 384 KiB
            }


data ConfigFile = ConfigFile
    { cardanoProtocols :: CardanoProtocolsConfigFile
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake ConfigFile


data CardanoProtocolsConfigFile = CardanoProtocolsConfigFile
    { blockFetch :: Config
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake CardanoProtocolsConfigFile


runConfig :: (IOE :> es, Reader ConfigPath :> es) => Eff (Reader Config : es) a -> Eff es a
runConfig eff = do
    configPath <- ask
    configFile <- loadYaml @ConfigFile configPath
    runReader configFile.cardanoProtocols.blockFetch eff
