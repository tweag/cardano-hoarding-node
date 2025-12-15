module Hoard.Data.Header.Extract
    ( extractHeaderData
    )
where

import Cardano.Api.LedgerState ()
import Ouroboros.Consensus.Block (BlockNo (..), SlotNo (..))
import Ouroboros.Consensus.Block.Abstract (blockNo, blockSlot)

import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.Header (Header (..))
import Hoard.Network.Events (HeaderReceivedData (..))


-- | Extract header data from a HeaderReceivedData event
extractHeaderData :: HeaderReceivedData -> Header
extractHeaderData dat =
    Header
        { hash = blockHashFromHeader dat.header
        , slotNumber = unSlotNo $ blockSlot dat.header
        , blockNumber = unBlockNo $ blockNo dat.header
        , firstSeenAt = dat.timestamp
        }
