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
import Hoard.Network.Events (BlockReceivedData (..))


-- | Extract block data from a BlockReceivedData event. Assumes the block is
-- not in the canonical chain and has not been validated.
extractBlockData :: BlockReceivedData -> Block
extractBlockData dat =
    Block
        { hash = blockHashFromHeader $ getHeader dat.block
        , slotNumber = fromIntegral $ unSlotNo $ blockSlot $ dat.block
        , poolId = mkPoolID dat.block
        , blockData = dat.block
        , validationStatus = "" -- Block has yet to be validated
        , validationReason = "" -- Block has yet to be validated
        , isCanonical = False -- Default to False until proven otherwise.
        , firstSeen = dat.timestamp
        }
