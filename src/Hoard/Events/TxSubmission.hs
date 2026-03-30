module Hoard.Events.TxSubmission
    ( TxReceived (..)
    ) where

import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)

import Hoard.Data.Peer (Peer)
import Hoard.Types.Cardano (CardanoBlock)


data TxReceived = TxReceived
    { peer :: Peer
    , tx :: GenTx CardanoBlock
    }
