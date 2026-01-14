module Hoard.Data.Header.Extract
    ( extractHeaderData
    )
where

import Cardano.Api.LedgerState ()
import Ouroboros.Consensus.Block (BlockNo (..), SlotNo (..))
import Ouroboros.Consensus.Block.Abstract (blockNo, blockSlot)

import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.Header (Header (..))
import Hoard.Network.Events (HeaderReceived (..))


-- | Extract header data from a HeaderReceived event
extractHeaderData :: HeaderReceived -> Header
extractHeaderData event =
    Header
        { hash = blockHashFromHeader event.header
        , slotNumber = unSlotNo $ blockSlot event.header
        , blockNumber = unBlockNo $ blockNo event.header
        , firstSeenAt = event.timestamp
        }
