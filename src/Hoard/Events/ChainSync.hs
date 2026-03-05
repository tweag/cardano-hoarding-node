module Hoard.Events.ChainSync
    ( HeaderReceived (..)
    , RollBackward (..)
    , RollForward (..)
    , IntersectionFound (..)
    ) where

import Cardano.Api.LedgerState ()

import Hoard.Data.Peer (Peer)
import Hoard.Types.Cardano (CardanoHeader, CardanoPoint, CardanoTip)


-- | Events from the ChainSync mini-protocol.
--
-- ChainSync is responsible for synchronizing chain headers with peers,
-- handling forks and rollbacks.
data HeaderReceived = HeaderReceived
    { peer :: Peer
    , header :: CardanoHeader
    , tip :: CardanoTip
    }
    deriving (Typeable)


data RollBackward = RollBackward
    { peer :: Peer
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Typeable)


data RollForward = RollForward
    { peer :: Peer
    , header :: CardanoHeader
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Typeable)


data IntersectionFound = IntersectionFound
    { peer :: Peer
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Typeable)
