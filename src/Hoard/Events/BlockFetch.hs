module Hoard.Events.BlockFetch
    ( Request (..)
    , RequestStarted (..)
    , BlockReceived (..)
    , RequestFailed (..)
    , BatchCompleted (..)
    ) where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange)

import Hoard.Data.Peer (Peer (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader, CardanoPoint)


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
    , requestId :: UUID
    , range :: ChainRange CardanoPoint
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
