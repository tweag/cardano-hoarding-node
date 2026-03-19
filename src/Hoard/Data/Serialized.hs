module Hoard.Data.Serialized
    ( Serialized (..)

      -- * Block
    , serializeBlock
    , deserializeBlock

      -- * Header
    , serializeHeader
    , deserializeHeader
    ) where

import Cardano.Api (EpochSlots (..))
import Codec.CBOR.Read (DeserialiseFailure (..))
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock
    , decodeByronBlock
    , encodeByronBlock
    )
import Ouroboros.Consensus.Cardano.Block
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ConwayEra
    , DijkstraEra
    , HardForkBlock (..)
    , MaryEra
    , ShelleyEra
    )
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock
    , ShelleyCompatible
    , decodeShelleyBlock
    , decodeShelleyHeader
    , encodeShelleyBlock
    , encodeShelleyHeader
    )
import Rel8 (DBEq, DBType)

import Cardano.Chain.Block qualified as CC
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Ouroboros.Consensus.Byron.Ledger qualified as Byron
import Ouroboros.Consensus.Cardano.Block qualified as O

import Hoard.Data.Eras (BlockEra (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader, Crypto)


newtype Serialized a = Serialized ByteString
    deriving stock (Eq, Generic, Show)
    deriving (DBEq, DBType) via ByteString


--------
-- Block
--------

serializeBlock :: CardanoBlock -> Serialized CardanoBlock
serializeBlock =
    Serialized . CBOR.toStrictByteString . \case
        O.BlockAllegra b -> encodeShelleyBlock b
        O.BlockAlonzo b -> encodeShelleyBlock b
        O.BlockMary b -> encodeShelleyBlock b
        O.BlockShelley b -> encodeShelleyBlock b
        O.BlockBabbage b -> encodeShelleyBlock b
        O.BlockConway b -> encodeShelleyBlock b
        O.BlockDijkstra b -> encodeShelleyBlock b
        O.BlockByron b -> encodeByronBlock b


-- | Decodes a `CardanoBlock` from a ByteString using CBOR. Must be able to
-- decode the output of @encodeCardanoBlock@.
deserializeBlock :: BlockEra -> Serialized CardanoBlock -> Either Text CardanoBlock
deserializeBlock blockEra (Serialized block) =
    first show $ case blockEra of
        Allegra -> BlockAllegra <$> deserialise @(TPraos Crypto) @AllegraEra block
        Alonzo -> BlockAlonzo <$> deserialise @(TPraos Crypto) @AlonzoEra block
        Mary -> BlockMary <$> deserialise @(TPraos Crypto) @MaryEra block
        Shelley -> BlockShelley <$> deserialise @(TPraos Crypto) @ShelleyEra block
        Babbage -> BlockBabbage <$> deserialise @(Praos Crypto) @BabbageEra block
        Conway -> BlockConway <$> deserialise @(Praos Crypto) @ConwayEra block
        Dijkstra -> BlockDijkstra <$> deserialise @(Praos Crypto) @DijkstraEra block
        Byron -> BlockByron <$> deserialiseByron block
  where
    deserialise :: forall proto era. (ShelleyCompatible proto era) => ByteString -> Either DeserialiseFailure (ShelleyBlock proto era)
    deserialise bs = do
        let lazyBs = toLazy bs
        (_, mkBlock) <- CBOR.deserialiseFromBytes (decodeShelleyBlock @proto @era) lazyBs

        -- The result of `decodeShelleyBlock` expects the original ByteString
        -- the block was deserialised from for verification purposes.
        pure $ mkBlock lazyBs

    deserialiseByron :: ByteString -> Either DeserialiseFailure ByronBlock
    deserialiseByron bs = do
        let lazyBs = toLazy bs
        -- TODO: Replace this EpochSlots with a more proper size, preferably
        -- one read from config. Debatable whether it's necessary, since we
        -- don't really expect to support Byron.
        (_, mkBlock) <- CBOR.deserialiseFromBytes (decodeByronBlock (EpochSlots 1000)) lazyBs
        -- The result of `decodeByronBlock` expects the original ByteString
        -- the block was deserialised from for verification purposes.
        pure $ mkBlock lazyBs


---------
-- Header
---------

serializeHeader :: CardanoHeader -> Serialized CardanoHeader
serializeHeader =
    Serialized . CBOR.toStrictByteString . \case
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
deserializeHeader :: BlockEra -> Serialized CardanoHeader -> Either Text CardanoHeader
deserializeHeader era (Serialized header) =
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
