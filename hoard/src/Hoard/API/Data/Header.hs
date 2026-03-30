module Hoard.API.Data.Header (Header (..), fromHeader) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time (UTCTime)

import Atelier.Types.QuietSnake (QuietSnake (..))
import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.Eras (BlockEra)
import Hoard.Data.Serialized (Serialized, serializeHeader)
import Hoard.Types.Cardano (CardanoHeader)
import Prelude hiding (id)

import Hoard.Data.Eras qualified as Era
import Hoard.Data.Header qualified as Header


-- | API stable representation of a header.
data Header = Header
    { hash :: BlockHash
    , headerEra :: BlockEra
    , headerData :: Serialized CardanoHeader
    , slotNumber :: Word64
    , blockNumber :: Word64
    , firstSeenAt :: UTCTime
    }
    deriving stock (Eq, Generic, Show)


deriving via QuietSnake Header instance (FromJSON Header)
deriving via QuietSnake Header instance (ToJSON Header)


fromHeader :: Header.Header -> Header
fromHeader b =
    Header
        { hash = b.hash
        , headerEra = Era.headerToEra b.headerData
        , headerData = serializeHeader b.headerData
        , slotNumber = b.slotNumber
        , blockNumber = b.blockNumber
        , firstSeenAt = b.firstSeenAt
        }
