module Hoard.Events.BlockFetch
    ( Request (..)
    , RequestStarted (..)
    , BlockReceived (..)
    , RequestFailed (..)
    , BatchCompleted (..)
    ) where

import Data.UUID (UUID)
import Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange)

import Hoard.Data.Peer (Peer (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader, CardanoPoint)


-- | Events from the BlockFetch mini-protocol.
--
-- BlockFetch is responsible for downloading block bodies after ChainSync
-- has provided the headers.
newtype RequestStarted = RequestStarted
    { peer :: Peer
    }
    deriving (Show, Typeable)


-- | A request to fetch a single block.
data Request = Request
    { peer :: Peer
    , header :: CardanoHeader
    }
    deriving (Eq, Show, Typeable)


data BlockReceived = BlockReceived
    { peer :: Peer
    , block :: CardanoBlock
    , requestId :: UUID
    , range :: ChainRange CardanoPoint
    }
    deriving (Typeable)


data RequestFailed = RequestFailed
    { peer :: Peer
    , header :: CardanoHeader
    , errorMessage :: Text
    }
    deriving (Typeable)


data BatchCompleted = BatchCompleted
    { peer :: Peer
    , blockCount :: Int
    }
    deriving (Show, Typeable)
