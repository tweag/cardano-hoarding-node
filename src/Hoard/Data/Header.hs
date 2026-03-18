module Hoard.Data.Header
    ( Header (..)
    , encodeCardanoHeader
    , decodeCardanoHeader
    , HeaderReceipt (..)
    )
where

import Cardano.Api.Consensus (EpochSlots (..))
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

import Cardano.Chain.Block qualified as CC
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Ouroboros.Consensus.Byron.Ledger.Block qualified as Byron
import Ouroboros.Consensus.Byron.Ledger.Serialisation qualified as Byron
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
        O.HeaderByron b -> encodeByronHeader b
  where
    encodeByronHeader b = case b.byronHeaderRaw of
        CC.ABOBBlockHdr regularHeader -> Byron.encodeByronRegularHeader regularHeader
        CC.ABOBBoundaryHdr boundaryHeader -> Byron.encodeByronBoundaryHeader boundaryHeader


-- | Decodes a `CardanoHeader` from a ByteString using CBOR. Must be able to
-- decode the output of @encodeCardanoHeader@.
decodeCardanoHeader :: BlockEra -> ByteString -> Either Text CardanoHeader
decodeCardanoHeader era header = do
    first show $ case era of
        Allegra -> O.HeaderAllegra <$> deserialise @(TPraos Crypto) @AllegraEra header
        Alonzo -> O.HeaderAlonzo <$> deserialise @(TPraos Crypto) @AlonzoEra header
        Mary -> O.HeaderMary <$> deserialise @(TPraos Crypto) @MaryEra header
        Shelley -> O.HeaderShelley <$> deserialise @(TPraos Crypto) @ShelleyEra header
        Babbage -> O.HeaderBabbage <$> deserialise @(Praos Crypto) @BabbageEra header
        Conway -> O.HeaderConway <$> deserialise @(Praos Crypto) @ConwayEra header
        Dijkstra -> O.HeaderDijkstra <$> deserialise @(Praos Crypto) @DijkstraEra header
        Byron -> O.HeaderByron <$> deserialiseByron header
  where
    deserialise :: forall proto era. (ShelleyCompatible proto era) => ByteString -> Either DeserialiseFailure (O.Header (ShelleyBlock proto era))
    deserialise bs = do
        let lazyBs = toLazy bs
        (_, mkHeader) <- CBOR.deserialiseFromBytes (decodeShelleyHeader @proto @era) lazyBs
        -- The result of `decodeShelleyHeader` expects the original ByteString
        -- the block was deserialised from for verification purposes.
        pure $ mkHeader lazyBs

    deserialiseByron :: ByteString -> Either DeserialiseFailure (O.Header Byron.ByronBlock)
    deserialiseByron bs = do
        let lazyBs = toLazy bs
        (_, mkHeaderData) <- case (CBOR.deserialiseFromBytes decodeRegularHeader lazyBs, CBOR.deserialiseFromBytes decodeBoundaryHeader lazyBs) of
            (Right x, _) -> Right x
            (_, Right y) -> Right y
            (Left (DeserialiseFailure offset reason1), Left (DeserialiseFailure _ reason2)) -> Left $ DeserialiseFailure offset $ reason1 <> ", " <> reason2
        let headerData = mkHeaderData lazyBs

            -- As per this comment:
            -- https://github.com/IntersectMBO/ouroboros-consensus/blob/f6f20d051ca0ce970b151fb747a0fe2e7dc31e8d/ouroboros-consensus-cardano/src/byron/Ouroboros/Consensus/Byron/Ledger/Block.hs#L141
            --
            -- > This is used only for the block fetch client. When this value is
            -- > wrong, block fetch might make suboptimal decisions, but it shouldn't
            -- > /break/ anythingkHeaderData lazyBs
            blockSizeHint = Byron.fakeByronBlockSizeHint
        pure $ Byron.mkByronHeader (EpochSlots 21600) headerData blockSizeHint
      where
        decodeRegularHeader = (CC.ABOBBlockHdr .) <$> Byron.decodeByronRegularHeader (EpochSlots 21600)
        decodeBoundaryHeader = (CC.ABOBBoundaryHdr .) <$> Byron.decodeByronBoundaryHeader


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
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
