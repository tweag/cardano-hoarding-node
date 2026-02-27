module Hoard.Data.Header
    ( Header (..)
    , encodeCardanoHeader
    , decodeCardanoHeader
    , HeaderReceipt (..)
    )
where

import Codec.CBOR.Read (DeserialiseFailure (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time (UTCTime)
import Ouroboros.Consensus.Cardano.Block
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ConwayEra
    , DijkstraEra
    , MaryEra
    , ShelleyEra
    )
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, ShelleyCompatible, decodeShelleyHeader, encodeShelleyHeader)
import System.IO.Error (userError)

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Ouroboros.Consensus.Cardano.Block qualified as O

import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.Eras (BlockEra (..))
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)
import Hoard.Types.Cardano (CardanoHeader, Crypto)
import Prelude hiding (id)


-- | Represents a block header from the Cardano blockchain
--
-- Stores unique headers indexed by block hash. In Cardano, the block hash
-- is the hash of the header itself, which uniquely identifies the block.
data Header = Header
    { hash :: BlockHash
    , headerData :: CardanoHeader
    , slotNumber :: Word64
    , blockNumber :: Word64
    , firstSeenAt :: UTCTime
    }
    deriving stock (Eq, Generic, Show)


encodeCardanoHeader :: CardanoHeader -> ByteString
encodeCardanoHeader =
    CBOR.toStrictByteString . \case
        O.HeaderAllegra b -> encodeShelleyHeader b
        O.HeaderAlonzo b -> encodeShelleyHeader b
        O.HeaderMary b -> encodeShelleyHeader b
        O.HeaderShelley b -> encodeShelleyHeader b
        O.HeaderBabbage b -> encodeShelleyHeader b
        O.HeaderConway b -> encodeShelleyHeader b
        O.HeaderDijkstra b -> encodeShelleyHeader b
        O.HeaderByron _ -> bug $ userError "No encoding for Byron headers"


-- | Decodes a `CardanoHeader` from a ByteString using CBOR. Must be able to
-- decode the output of @encodeCardanoHeader@.
decodeCardanoHeader :: BlockEra -> ByteString -> Either Text CardanoHeader
decodeCardanoHeader blockEra block = do
    first show $ case blockEra of
        Allegra -> O.HeaderAllegra <$> deserialise @(TPraos Crypto) @AllegraEra block
        Alonzo -> O.HeaderAlonzo <$> deserialise @(TPraos Crypto) @AlonzoEra block
        Mary -> O.HeaderMary <$> deserialise @(TPraos Crypto) @MaryEra block
        Shelley -> O.HeaderShelley <$> deserialise @(TPraos Crypto) @ShelleyEra block
        Babbage -> O.HeaderBabbage <$> deserialise @(Praos Crypto) @BabbageEra block
        Conway -> O.HeaderConway <$> deserialise @(Praos Crypto) @ConwayEra block
        Dijkstra -> O.HeaderDijkstra <$> deserialise @(Praos Crypto) @DijkstraEra block
        Byron -> Left $ DeserialiseFailure 0 "unable to deserialise Byron headers"
  where
    deserialise :: forall proto era. (ShelleyCompatible proto era) => ByteString -> Either DeserialiseFailure (O.Header (ShelleyBlock proto era))
    deserialise bs = do
        let lazyBs = toLazy bs
        (_, mkHeader) <- CBOR.deserialiseFromBytes (decodeShelleyHeader @proto @era) lazyBs
        -- The result of `decodeShelleyHeader` expects the original ByteString
        -- the block was deserialised from for verification purposes.
        pure $ mkHeader lazyBs


-- | Represents a receipt of a header from a specific peer
--
-- Tracks each time a header was received from a peer, enabling
-- many-to-many relationship between headers and peers.
data HeaderReceipt = HeaderReceipt
    { id :: ID HeaderReceipt
    , hash :: BlockHash
    , peerId :: ID Peer
    , receivedAt :: UTCTime
    }
    deriving (FromJSON, ToJSON)
    deriving stock (Eq, Generic, Show)
