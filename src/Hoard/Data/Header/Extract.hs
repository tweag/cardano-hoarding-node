module Hoard.Data.Header.Extract
    ( extractHeaderData
    )
where

import Data.Time (UTCTime)

import Hoard.Data.Header (BlockHash (..), Header (..))
import Hoard.Types.Cardano (CardanoHeader)


-- | Extract header data from a Cardano header
--
-- Currently uses simple string representation via Show instance.
-- TODO: Implement proper extraction of slot/block numbers and hash using era-specific accessors
extractHeaderData :: CardanoHeader -> UTCTime -> Header
extractHeaderData _cardanoHeader receivedAt =
    let
        -- TODO: Extract actual hash from era-specific header
        -- All HardForkBlock operations require CanHardFork instance which is complex to set up
        blockHash = BlockHash "placeholder-hash"

        -- TODO: Extract actual slot number from header
        -- Requires handling HardForkBlock's OneEraHeader structure
        slotNumber = 0

        -- TODO: Extract actual block number from header
        -- Requires handling HardForkBlock's OneEraHeader structure
        blockNumber = 0

        vrfKeyHash = Nothing -- TODO: Extract from era-specific header body
        blockTimestamp = Nothing -- TODO: Derive from slot + node config
    in
        Header
            { blockHash
            , slotNumber
            , blockNumber
            , vrfKeyHash
            , blockTimestamp
            , firstSeenAt = receivedAt
            }
