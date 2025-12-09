module Hoard.Data.Header.Extract
    ( extractHeaderData
    )
where

import Data.Time (UTCTime)

import Cardano.Api.LedgerState ()
import Hoard.Data.Header (BlockHash (..), Header (..))
import Hoard.Types.Cardano (CardanoHeader)
import Ouroboros.Consensus.Block (BlockNo (..), SlotNo (..))
import Ouroboros.Consensus.Block.Abstract
    ( HasHeader (getHeaderFields)
    , HeaderFields (..)
    )


-- | Extract header data from a Cardano header
--
-- Currently uses simple string representation via Show instance.
extractHeaderData :: CardanoHeader -> UTCTime -> Header
extractHeaderData cardanoHeader firstSeenAt =
    let
        fields = getHeaderFields cardanoHeader

        -- Extract hash and convert to Text using Show instance
        blockHash = BlockHash $ show fields.headerFieldHash

        -- Extract slot number and convert Word64 to Int64
        slotNumber = fromIntegral . unSlotNo $ fields.headerFieldSlot

        -- Extract block number and convert Word64 to Int64
        blockNumber = fromIntegral . unBlockNo $ fields.headerFieldBlockNo

        -- TODO: Extract from era-specific header body
        vrfKeyHash = Nothing

        -- TODO: Derive from slot + node config
        blockTimestamp = Nothing
    in
        Header
            { blockHash
            , slotNumber
            , blockNumber
            , vrfKeyHash
            , blockTimestamp
            , firstSeenAt
            }
