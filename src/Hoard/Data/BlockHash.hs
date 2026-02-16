module Hoard.Data.BlockHash
    ( BlockHash (..)
    , renderHash
    , blockHashFromHeader
    ) where

import Cardano.Api.LedgerState ()
import Cardano.Ledger.Keys (CertifiedVRF)
import Cardano.Protocol.Crypto (Crypto (VRF))
import Cardano.Protocol.TPraos.BHeader (BHBody (BHBody, bheaderEta, bheaderL), BHeader (BHeader))
import Data.Aeson (FromJSON, ToJSON)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash, toRawHash)
import Ouroboros.Consensus.Cardano (Nonce)
import Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock, CardanoHeader)
import Ouroboros.Consensus.Protocol.Praos.Header (Header (Header), HeaderBody (HeaderBody, hbVrfRes))
import Ouroboros.Consensus.Protocol.Praos.Views (HeaderView (HeaderView, hvVrfRes))
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger (Header (ShelleyHeader, shelleyHeaderRaw), ShelleyBlock)
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtocolHeaderSupportsProtocol (protocolHeaderView))
import Ouroboros.Network.Block (HeaderHash, blockHash)
import Rel8 (DBEq, DBOrd, DBType)

import Data.ByteString.Base16 qualified as B16
import Data.Text.Encoding qualified as Text

import Hoard.Effects.Monitoring.Tracing (ToAttribute, ToAttributeShow (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)


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
t header =
    let
        a =
            case header of
                HeaderShelley h ->
                    case h of ShelleyHeader {shelleyHeaderRaw} -> case shelleyHeaderRaw of BHeader (BHBody {bheaderEta}) _signed -> Left bheaderEta
                HeaderAllegra h ->
                    case h of ShelleyHeader {shelleyHeaderRaw} -> case shelleyHeaderRaw of BHeader (BHBody {bheaderEta}) _signed -> Left bheaderEta
                HeaderMary h ->
                    case h of ShelleyHeader {shelleyHeaderRaw} -> case shelleyHeaderRaw of BHeader (BHBody {bheaderEta}) _signed -> Left bheaderEta
                HeaderAlonzo h ->
                    case h of ShelleyHeader {shelleyHeaderRaw} -> case shelleyHeaderRaw of BHeader (BHBody {bheaderEta}) _signed -> Left bheaderEta
                HeaderBabbage h ->
                    case h of ShelleyHeader {shelleyHeaderRaw} -> case shelleyHeaderRaw of Header (HeaderBody {hbVrfRes}) _signed -> Right hbVrfRes
                HeaderConway h ->
                    if False then
                        case h of ShelleyHeader {shelleyHeaderRaw} -> case protocolHeaderView shelleyHeaderRaw of HeaderView {hvVrfRes} -> Right hvVrfRes
                    else
                        case h of ShelleyHeader {shelleyHeaderRaw} -> case shelleyHeaderRaw of Header (HeaderBody {hbVrfRes}) _signed -> Right hbVrfRes
                HeaderDijkstra h ->
                    case h of ShelleyHeader {shelleyHeaderRaw} -> case shelleyHeaderRaw of Header (HeaderBody {hbVrfRes}) _signed -> Right hbVrfRes
                HeaderByron _h -> error "to do"
    in
        ()


headerEta :: (Crypto c) => Ouroboros.Consensus.Shelley.Ledger.Header (ShelleyBlock (TPraos c) era) -> CertifiedVRF (VRF c) Nonce
headerEta (ShelleyHeader {shelleyHeaderRaw}) = case shelleyHeaderRaw of BHeader (BHBody {bheaderEta, bheaderL}) _signed -> undefined
