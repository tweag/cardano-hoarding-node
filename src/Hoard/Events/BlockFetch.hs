module Hoard.Events.BlockFetch
    ( Request (..)
    , RequestStarted (..)
    , BlockReceived (..)
    , RequestFailed (..)
    , BatchFailed (..)
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
    deriving (Show)


-- | A request to fetch a single block.
data Request = Request
    { peer :: Peer
    , header :: CardanoHeader
    }
    deriving (Eq, Show)


data BlockReceived = BlockReceived
    { peer :: Peer
    , block :: CardanoBlock
    , requestId :: UUID
    , range :: ChainRange CardanoPoint
    , headerWithSameSlotNumber :: Maybe CardanoHeader
    }


data RequestFailed = RequestFailed
    { peer :: Peer
    , header :: CardanoHeader
    , errorMessage :: Text
    }


data BatchFailed = BatchFailed
    { peer :: Peer
    , headers :: NonEmpty CardanoHeader
    , errorMessage :: Text
    }


data BatchCompleted = BatchCompleted
    { peer :: Peer
    , blockCount :: Int
    }
    deriving (Show)
