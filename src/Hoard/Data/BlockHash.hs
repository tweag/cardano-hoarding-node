module Hoard.Data.BlockHash
    ( BlockHash (..)
    , renderHash
    , blockHashFromHeader
    ) where

import Cardano.Api.LedgerState ()
import Data.Aeson (FromJSON, ToJSON)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash, toRawHash)
import Ouroboros.Network.Block (HeaderHash, blockHash)
import Rel8 (DBEq, DBOrd, DBType)

import Data.ByteString.Base16 qualified as B16
import Data.Text.Encoding qualified as Text

import Hoard.Effects.Monitoring.Tracing (ToAttribute, ToAttributeShow (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)
import Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock, CardanoHeader)
import Ouroboros.Consensus.HardFork.Combinator


-- | Newtype wrapper for block hash
newtype BlockHash = BlockHash Text
    deriving stock (Eq, Generic, Ord, Show)
    deriving (DBEq, DBOrd, DBType, FromJSON, Hashable, ToJSON) via Text
    deriving (ToAttribute) via ToAttributeShow BlockHash


blockHashFromHeader :: CardanoHeader -> BlockHash
blockHashFromHeader = BlockHash . renderHash (Proxy @CardanoBlock) . blockHash


-- | Hex encode and render a 'HeaderHash' as text.
-- This is done similarly inside the cardano-node codebase.
renderHash :: (ConvertRawHash blk) => proxy blk -> HeaderHash blk -> Text
renderHash p = Text.decodeLatin1 . B16.encode . toRawHash p


t :: CardanoHeader -> ()
t h =
    let
        a =
            case h of
                HeaderShelley _h -> undefined
                HeaderAllegra _h -> undefined
                HeaderMary _h -> undefined
                HeaderAlonzo _h -> undefined
                HeaderBabbage _h -> undefined
                HeaderConway h -> h
                HeaderDijkstra _h -> undefined
                HeaderByron _h -> undefined
    in
        ()
