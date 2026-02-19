module Hoard.BlockEviction.Config
    ( Config (..)
    , runBlockEvictionConfig
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static (Reader, ask, runReader)
import Prelude hiding (Reader, ask, runReader)

import Hoard.Effects.ConfigPath (ConfigPath, loadYaml)
import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Configuration for block eviction
data Config = Config
    { evictionIntervalSeconds :: !Int
    -- ^ How often the eviction trigger runs (in seconds).
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { evictionIntervalSeconds = 3600 -- 1 hour
            }


data ConfigFile = ConfigFile
    { blockEviction :: Config
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake ConfigFile


runBlockEvictionConfig :: (IOE :> es, Reader ConfigPath :> es) => Eff (Reader Config : es) a -> Eff es a
runBlockEvictionConfig eff = do
    configPath <- ask
    configFile <- loadYaml @ConfigFile configPath
    runReader configFile.blockEviction eff
