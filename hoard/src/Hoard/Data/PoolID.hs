module Hoard.Data.PoolID
    ( PoolID (..)
    , mkPoolID
    ) where

import Cardano.Api (Hash (..))
import Cardano.Api.Serialise.Raw (serialiseToRawBytesHexText)
import Cardano.Ledger.Keys (coerceKeyRole, hashKey)
import Data.Aeson (FromJSON, ToJSON)
import Ouroboros.Consensus.Block (GetHeader (..))
import Ouroboros.Consensus.Cardano.Block (Header (..))
import Ouroboros.Consensus.Cardano.CanHardFork ()
import Ouroboros.Consensus.Shelley.Ledger (Header (shelleyHeaderRaw))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtocolHeaderSupportsProtocol, pHeaderIssuer)
import Rel8 (DBEq, DBOrd, DBType)

import Hoard.Types.Cardano (CardanoBlock)


newtype PoolID = PoolID Text
    deriving stock (Eq, Generic, Ord, Show)
    deriving newtype (DBEq, DBOrd, DBType, FromJSON, ToJSON)


mkPoolID :: CardanoBlock -> PoolID
mkPoolID blk = case getHeader blk of
    HeaderByron _ -> PoolID "byron" -- Byron does not have PoolIDs
    HeaderAllegra h -> shelleyHeaderToPoolID h
    HeaderAlonzo h -> shelleyHeaderToPoolID h
    HeaderMary h -> shelleyHeaderToPoolID h
    HeaderShelley h -> shelleyHeaderToPoolID h
    HeaderBabbage h -> shelleyHeaderToPoolID h
    HeaderConway h -> shelleyHeaderToPoolID h
    HeaderDijkstra h -> shelleyHeaderToPoolID h
  where
    shelleyHeaderToPoolID :: forall proto era. (ProtocolHeaderSupportsProtocol proto) => Header (ShelleyBlock proto era) -> PoolID
    shelleyHeaderToPoolID =
        PoolID
            . serialiseToRawBytesHexText
            . StakePoolKeyHash -- Creates an actual PoolId, as defined by Cardano
            . hashKey
            . coerceKeyRole
            . pHeaderIssuer
            . shelleyHeaderRaw
