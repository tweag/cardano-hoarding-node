module Hoard.Data.Block.Extract (extractBlockData) where

import Cardano.Api.LedgerState ()
import Ouroboros.Consensus.Block
    ( SlotNo (..)
    , blockSlot
    , getHeader
    )

import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.PoolID (mkPoolID)
import Hoard.Network.Events (BlockReceived (..))


-- | Extract block data from a BlockReceived event.
-- Assumes the block is not in the canonical chain and has not been validated.
extractBlockData :: BlockReceived -> Block
extractBlockData event =
    Block
        { hash = blockHashFromHeader $ getHeader event.block
        , slotNumber = fromIntegral $ unSlotNo $ blockSlot $ event.block
        , poolId = mkPoolID event.block
        , blockData = event.block
        , validationStatus = "" -- Block has yet to be validated
        , validationReason = "" -- Block has yet to be validated
        , isCanonical = False -- Default to False until proven otherwise.
        , firstSeen = event.timestamp
        }
