module Hoard.Events.ChainSync
    ( ChainSyncStarted (..)
    , HeaderReceived (..)
    , RollBackward (..)
    , RollForward (..)
    , ChainSyncIntersectionFound (..)
    ) where

import Cardano.Api.LedgerState ()
import Data.Time (UTCTime)

import Hoard.Data.Peer (Peer)
import Hoard.Types.Cardano (CardanoHeader, CardanoPoint, CardanoTip)


-- | Events from the ChainSync mini-protocol.
--
-- ChainSync is responsible for synchronizing chain headers with peers,
-- handling forks and rollbacks.
data ChainSyncStarted = ChainSyncStarted
    { peer :: Peer
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data HeaderReceived = HeaderReceived
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    , tip :: CardanoTip
    }
    deriving (Typeable)


data RollBackward = RollBackward
    { peer :: Peer
    , timestamp :: UTCTime
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Typeable)


data RollForward = RollForward
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Typeable)


data ChainSyncIntersectionFound = ChainSyncIntersectionFound
    { peer :: Peer
    , timestamp :: UTCTime
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Typeable)
