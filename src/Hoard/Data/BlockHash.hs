module Hoard.Data.BlockHash
    ( BlockHash (..)
    , renderHash
    , mkBlockHash
    ) where

import Cardano.Api.LedgerState ()
import Data.Aeson (FromJSON, ToJSON)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash, toRawHash)
import Ouroboros.Network.Block (HasHeader, HeaderHash, blockHash)
import Rel8 (DBEq, DBOrd, DBType)

import Data.ByteString.Base16 qualified as B16
import Data.Text.Encoding qualified as Text

import Hoard.Effects.Monitoring.Tracing (ToAttribute, ToAttributeShow (..))
import Hoard.Types.Cardano (CardanoBlock)


-- | Newtype wrapper for block hash
newtype BlockHash = BlockHash Text
    deriving stock (Eq, Generic, Ord, Show)
    deriving (DBEq, DBOrd, DBType, FromJSON, Hashable, ToJSON) via Text
    deriving (ToAttribute) via ToAttributeShow BlockHash


mkBlockHash :: (HasHeader blk, HeaderHash blk ~ HeaderHash CardanoBlock) => blk -> BlockHash
mkBlockHash = BlockHash . renderHash (Proxy @CardanoBlock) . blockHash


-- | Hex encode and render a 'HeaderHash' as text.
-- This is done similarly inside the cardano-node codebase.
renderHash :: (ConvertRawHash blk) => proxy blk -> HeaderHash blk -> Text
renderHash p = Text.decodeLatin1 . B16.encode . toRawHash p
