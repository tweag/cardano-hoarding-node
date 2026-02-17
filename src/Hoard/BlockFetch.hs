module Hoard.BlockFetch
    ( -- * Events
      Request (..)
    , RequestStarted (..)
    , BlockReceived (..)
    , RequestFailed (..)
    , BatchCompleted (..)
    ) where

import Data.Time (UTCTime)
import Prelude hiding (Reader, State, ask, evalState, get, modify, runReader)

import Hoard.Data.Peer (Peer (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)


-- | Events from the BlockFetch mini-protocol.
--
-- BlockFetch is responsible for downloading block bodies after ChainSync
-- has provided the headers.
data RequestStarted = RequestStarted
    { peer :: Peer
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


-- | A request to fetch a single block.
data Request = Request
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    }
    deriving (Eq, Show, Typeable)


data BlockReceived = BlockReceived
    { peer :: Peer
    , timestamp :: UTCTime
    , block :: CardanoBlock
    }
    deriving (Typeable)


data RequestFailed = RequestFailed
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    , errorMessage :: Text
    }
    deriving (Typeable)


data BatchCompleted = BatchCompleted
    { peer :: Peer
    , timestamp :: UTCTime
    , blockCount :: Int
    }
    deriving (Show, Typeable)
