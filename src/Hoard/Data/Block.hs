module Hoard.Data.Block
    ( Block (..)
    , encodeCardanoBlock
    , decodeCardanoBlock
    ) where

import Cardano.Api (EpochSlots (..))
import Codec.CBOR.Read (DeserialiseFailure)
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Data.Time (UTCTime)
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
    , encodeShelleyBlock
    )

import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.Eras (BlockEra (..))
import Hoard.Types.Cardano (CardanoBlock, Crypto)


data Block = Block
    { hash :: BlockHash
    , slotNumber :: Int64
    , poolId :: Text
    , blockData :: CardanoBlock
    , validationStatus :: Text
    , validationReason :: Text
    , isCanonical :: Bool
    , firstSeen :: UTCTime
    }
    deriving (Eq)


encodeCardanoBlock :: CardanoBlock -> ByteString
encodeCardanoBlock =
    CBOR.toStrictByteString . \case
        BlockAllegra b -> encodeShelleyBlock b
        BlockAlonzo b -> encodeShelleyBlock b
        BlockMary b -> encodeShelleyBlock b
        BlockShelley b -> encodeShelleyBlock b
        BlockBabbage b -> encodeShelleyBlock b
        BlockConway b -> encodeShelleyBlock b
        BlockDijkstra b -> encodeShelleyBlock b
        BlockByron b -> encodeByronBlock b


-- | Decodes a `CardanoBlock` from a ByteString using CBOR. Must be able to
-- decode the output of @encodeCardanoBlock@.
decodeCardanoBlock :: BlockEra -> ByteString -> Either Text CardanoBlock
decodeCardanoBlock blockEra block = do
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
