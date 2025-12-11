module Hoard.Data.Header.Extract
    ( extractHeaderData
    )
where

import Data.ByteString.Base16 qualified as B16
import Data.Text.Encoding qualified as Text

import Cardano.Api.LedgerState ()
import Hoard.Data.Header (BlockHash (..), Header (..))
import Hoard.Network.Events (HeaderReceivedData (..))
import Hoard.Types.Cardano (CardanoBlock)
import Ouroboros.Consensus.Block (BlockNo (..), SlotNo (..))
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (toRawHash), blockNo, blockSlot)
import Ouroboros.Consensus.Block.Abstract qualified as Block
import Ouroboros.Network.Block (HeaderHash)


-- | Extract header data from a HeaderReceivedData event
extractHeaderData :: HeaderReceivedData -> Header
extractHeaderData dat =
    let
        hash =
            BlockHash (renderHash (Proxy @CardanoBlock) (Block.blockHash dat.header))

        -- Extract slot number and convert Word64 to Int64
        slotNumber = unSlotNo $ blockSlot dat.header

        -- Extract block number and convert Word64 to Int64
        blockNumber = unBlockNo $ blockNo dat.header
    in
        Header
            { hash
            , slotNumber
            , blockNumber
            , firstSeenAt = dat.timestamp
            }


-- | Hex encode and render a 'HeaderHash' as text.
-- This is done similarly inside the cardano-node codebase.
renderHash :: (ConvertRawHash blk) => proxy blk -> HeaderHash blk -> Text
renderHash p = Text.decodeLatin1 . B16.encode . toRawHash p
