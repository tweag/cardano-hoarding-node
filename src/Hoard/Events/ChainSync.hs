module Hoard.Events.ChainSync
    ( HeaderReceived (..)
    , RollBackward (..)
    , RollForward (..)
    , IntersectionFound (..)
    ) where

import Cardano.Api.LedgerState ()
import Data.Time (UTCTime)

import Hoard.Data.Peer (Peer)
import Hoard.Types.Cardano (CardanoHeader, CardanoPoint, CardanoTip)


-- | Events from the ChainSync mini-protocol.
--
-- ChainSync is responsible for synchronizing chain headers with peers,
-- handling forks and rollbacks.
data HeaderReceived = HeaderReceived
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    , tip :: CardanoTip
    }
    deriving (Show, Typeable)


data RollBackward = RollBackward
    { peer :: Peer
    , timestamp :: UTCTime
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Show, Typeable)


data RollForward = RollForward
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Show, Typeable)


data IntersectionFound = IntersectionFound
    { peer :: Peer
    , timestamp :: UTCTime
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Show, Typeable)
