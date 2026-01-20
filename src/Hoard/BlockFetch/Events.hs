module Hoard.BlockFetch.Events
    ( BlockFetchRequest (..)
    , BlockFetchStarted (..)
    , BlockReceived (..)
    , BlockFetchFailed (..)
    , BlockBatchCompleted (..)
    ) where

import Data.Time (UTCTime)

import Hoard.Data.Peer (Peer)
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)


--------------------------------------------------------------------------------
-- BlockFetch Protocol Events
--------------------------------------------------------------------------------

-- | Events from the BlockFetch mini-protocol.
--
-- BlockFetch is responsible for downloading block bodies after ChainSync
-- has provided the headers.
data BlockFetchStarted = BlockFetchStarted
    { peer :: Peer
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


-- | A request to fetch a single block.
data BlockFetchRequest = BlockFetchRequest
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    }
    deriving (Typeable)


data BlockReceived = BlockReceived
    { peer :: Peer
    , timestamp :: UTCTime
    , block :: CardanoBlock
    }
    deriving (Typeable)


data BlockFetchFailed = BlockFetchFailed
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    , errorMessage :: Text
    }
    deriving (Typeable)


data BlockBatchCompleted = BlockBatchCompleted
    { peer :: Peer
    , timestamp :: UTCTime
    , blockCount :: Int
    }
    deriving (Show, Typeable)
