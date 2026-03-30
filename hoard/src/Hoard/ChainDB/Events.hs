module Hoard.ChainDB.Events
    ( BlockSealed (..)
    , BlockRolledBack (..)
    , BlockRejected (..)
    , ChainExtended (..)
    ) where

import Ouroboros.Consensus.Ledger.Extended (ExtValidationError)

import Hoard.Types.Cardano (CardanoBlock, CardanoPoint)


-- | A block has crossed the immutable boundary and can no longer be rolled back.
data BlockSealed = BlockSealed {point :: CardanoPoint}


-- | A block was on the canonical chain but has been rolled back by a fork.
data BlockRolledBack = BlockRolledBack {point :: CardanoPoint}


-- | The volatile chain was extended with a new block.
data ChainExtended = ChainExtended {point :: CardanoPoint}


-- | A block failed ChainDB validation.
data BlockRejected = BlockRejected
    { point :: CardanoPoint
    , validationError :: ExtValidationError CardanoBlock
    }
