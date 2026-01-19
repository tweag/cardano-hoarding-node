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
    , lowWatermark :: Int
    -- ^ Watermark for number of bytes in flight before BlockFetch will resume issuing block requests.
    , highWatermark :: Int
    -- ^ Watermark for number of bytes in flight after which BlockFetch will halt issuing block requests.
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { batchSize = 10
            , batchTimeoutMicroseconds = 10_000_000 -- 10 seconds
            , maximumIngressQueue = 589824 -- 576 KiB
            , lowWatermark = 196608 -- 192KiB
            , highWatermark = 393216 -- 384KiB
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
