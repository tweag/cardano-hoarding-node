module Hoard.Data.Header.Extract
    ( extractHeaderData
    )
where

import Data.ByteString.Base16 qualified as B16
import Data.Text.Encoding qualified as Text

import Cardano.Api.LedgerState ()
import Hoard.Data.Header (BlockHash (..), Header (..))
import Hoard.Data.Header qualified as Hoard
import Hoard.Network.Events (HeaderReceivedData (..))
import Hoard.Types.Cardano (CardanoBlock)
import Ouroboros.Consensus.Block (BlockNo (..), SlotNo (..))
import Ouroboros.Consensus.Block.Abstract
    ( ConvertRawHash (toRawHash)
    , HasHeader (getHeaderFields)
    , HeaderFields (..)
    )
import Ouroboros.Network.Block (HeaderHash)
import Ouroboros.Network.Block qualified as OBlock


-- | Extract header data from a HeaderReceivedData event
extractHeaderData :: HeaderReceivedData -> Header
extractHeaderData dat =
    let
        fields = getHeaderFields dat.header

        -- Extract header hash (hash of the header itself) from header fields
        headerHash =
            Hoard.HeaderHash (renderHash (Proxy @CardanoBlock) fields.headerFieldHash)

        -- Extract block hash using the blockHash function from HasHeader
        blockHash =
            BlockHash (renderHash (Proxy @CardanoBlock) (OBlock.blockHash dat.header))

        -- Extract slot number and convert Word64 to Int64
        slotNumber = unSlotNo $ fields.headerFieldSlot

        -- Extract block number and convert Word64 to Int64
        blockNumber = unBlockNo $ fields.headerFieldBlockNo
    in
        Header
            { headerHash
            , blockHash
            , slotNumber
            , blockNumber
            , firstSeenAt = dat.timestamp
            }


-- | Hex encode and render a 'HeaderHash' as text.
-- This is done similarly inside the cardano-node codebase.
renderHash :: (ConvertRawHash blk) => proxy blk -> HeaderHash blk -> Text
renderHash p = Text.decodeLatin1 . B16.encode . toRawHash p
