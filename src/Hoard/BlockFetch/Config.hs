module Hoard.BlockFetch.Config
    ( Config (..)
    , Handles (..)
    , HandlesConfig (..)
    , loadHandles
    ) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Effectful (Eff, (:>))
import Effectful.Concurrent.QSem (Concurrent, QSem, newQSem)
import Hoard.Types.QuietSnake (QuietSnake (..))


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


data HandlesConfig = HandlesConfig
    { maxConcurrentFetches :: Int
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake HandlesConfig


data Handles = Handles
    { qSem :: QSem
    }


loadHandles :: (Concurrent :> es) => HandlesConfig -> Eff es Handles
loadHandles conf = do
    qSem <- newQSem conf.maxConcurrentFetches
    pure Handles {qSem}
