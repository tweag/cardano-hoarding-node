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
import Ouroboros.Consensus.Block (BlockNo (..), HeaderHash, SlotNo (..))
import Ouroboros.Consensus.Block.Abstract
    ( ConvertRawHash (toRawHash)
    , HasHeader (getHeaderFields)
    , HeaderFields (..)
    )


-- | Extract header data from a HeaderReceivedData event
extractHeaderData :: HeaderReceivedData -> Header
extractHeaderData dat =
    let
        fields = getHeaderFields dat.header

        -- Extract hash and convert to hex-encoded Text
        blockHash =
            BlockHash (renderHeaderHash (Proxy @CardanoBlock) fields.headerFieldHash)

        -- Extract slot number and convert Word64 to Int64
        slotNumber = fromIntegral . unSlotNo $ fields.headerFieldSlot

        -- Extract block number and convert Word64 to Int64
        blockNumber = fromIntegral . unBlockNo $ fields.headerFieldBlockNo
    in
        Header
            { blockHash
            , slotNumber
            , blockNumber
            , firstSeenAt = dat.timestamp
            }


-- | Hex encode and render a 'HeaderHash' as text.
-- This is done similarly inside the cardano-node codebase.
renderHeaderHash :: (ConvertRawHash blk) => proxy blk -> HeaderHash blk -> Text
renderHeaderHash p = Text.decodeLatin1 . B16.encode . toRawHash p
