module Hoard.Effects.Verifier
    ( -- * Effect
      Verifier (..)
    , verifyCardanoHeader
    , verifyCardanoBlock
    , verifyBlock
    , verifyHeader
    , Validity (..)
    , Verified
    , getVerified

      -- * Interpreters
    , runVerifier
    , runAllValidVerifier
    , runCardanoConfigs
    , runShelleyConfig
    , runByronConfig
    ) where

import Cardano.Api
    ( ByronBlock
    , NodeConfig (..)
    , ShelleyConfig (..)
    , ShelleyGenesis (..)
    , readByronGenesisConfig
    , readShelleyGenesisConfig
    )
import Control.Monad.Trans.Except (runExceptT)
import Effectful (Effect, IOE)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Exception (throwIO)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.TH (makeEffect)
import Ouroboros.Consensus.Block (BlockConfig)
import Ouroboros.Consensus.Byron.Node (mkByronConfig)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, ShelleyCompatible, shelleyHeaderRaw)
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtocolHeaderSupportsKES)
import System.IO.Error (userError)

import Cardano.Api.Byron qualified as Byron
import Ouroboros.Consensus.Byron.Ledger.Integrity qualified as Byron
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley

import Hoard.Data.Block (Block (..))
import Hoard.Data.Header (Header (..))
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)


data Verifier :: Effect where
    -- | Verify that the header signature is correct and valid.
    VerifyCardanoHeader :: CardanoHeader -> Verifier m (VerificationResult CardanoHeader)
    VerifyCardanoBlock :: CardanoBlock -> Verifier m (VerificationResult CardanoBlock)
    VerifyBlock :: Block -> Verifier m (VerificationResult Block)
    VerifyHeader :: Header -> Verifier m (VerificationResult Header)


type VerificationResult a = Either (Verified 'Invalid a) (Verified 'Valid a)


data Validity = Valid | Invalid


newtype Verified (v :: Validity) a = Verified a


getVerified :: Verified v a -> a
getVerified (Verified a) = a


makeEffect ''Verifier


runVerifier
    :: forall es a
     . (Reader (BlockConfig ByronBlock) :> es, Reader ShelleyConfig :> es, Tracing :> es)
    => Eff (Verifier : es) a
    -> Eff es a
runVerifier = interpret_ \case
    VerifyCardanoHeader header -> withSpan "verifier.verify_cardano_header" $ verifyCardanoHeader' header
    VerifyCardanoBlock block -> withSpan "verifier.verify_cardano_block" $ verifyCardanoBlock' block
    VerifyBlock block -> withSpan "verifier.verify_block" do
        bimap (const $ Verified block) (const $ Verified block) <$> verifyCardanoBlock' block.blockData
    VerifyHeader header -> withSpan "verifier.verify_header" do
        bimap (const $ Verified header) (const $ Verified header) <$> verifyCardanoHeader' header.headerData
  where
    verifyCardanoBlock' block = do
        let verifyShelley :: (ShelleyCompatible proto era) => ShelleyBlock proto era -> Eff es (VerificationResult CardanoBlock)
            verifyShelley b = do
                shelleyConf <- ask @ShelleyConfig
                let spKES = shelleyConf.scConfig.sgSlotsPerKESPeriod
                if Shelley.verifyBlockIntegrity spKES b then
                    pure $ Right $ Verified block
                else
                    pure $ Left $ Verified block
        case block of
            O.BlockByron b -> do
                byronConf <- ask @(BlockConfig ByronBlock)
                if Byron.verifyBlockIntegrity byronConf b then
                    pure $ Right $ Verified block
                else
                    pure $ Left $ Verified block
            O.BlockShelley b -> verifyShelley b
            O.BlockAllegra b -> verifyShelley b
            O.BlockConway b -> verifyShelley b
            O.BlockBabbage b -> verifyShelley b
            O.BlockMary b -> verifyShelley b
            O.BlockAlonzo b -> verifyShelley b
            O.BlockDijkstra b -> verifyShelley b
    verifyCardanoHeader' header = do
        let verifyShelley
                :: (ProtocolHeaderSupportsKES proto)
                => O.Header (ShelleyBlock proto era)
                -> Eff es (VerificationResult CardanoHeader)
            verifyShelley h = do
                shelleyConf <- ask @ShelleyConfig
                let spKES = shelleyConf.scConfig.sgSlotsPerKESPeriod
                if Shelley.verifyHeaderIntegrity spKES (shelleyHeaderRaw h) then
                    pure $ Right $ Verified header
                else
                    pure $ Left $ Verified header
        case header of
            O.HeaderByron h -> do
                byronConf <- ask @(BlockConfig ByronBlock)
                if Byron.verifyHeaderIntegrity byronConf h then
                    pure $ Right $ Verified header
                else
                    pure $ Left $ Verified header
            O.HeaderShelley h -> verifyShelley h
            O.HeaderAllegra h -> verifyShelley h
            O.HeaderConway h -> verifyShelley h
            O.HeaderBabbage h -> verifyShelley h
            O.HeaderMary h -> verifyShelley h
            O.HeaderAlonzo h -> verifyShelley h
            O.HeaderDijkstra h -> verifyShelley h


runAllValidVerifier :: Eff (Verifier : es) a -> Eff es a
runAllValidVerifier = interpret_ \case
    VerifyCardanoHeader header -> pure $ Right $ Verified header
    VerifyCardanoBlock block -> pure $ Right $ Verified block
    VerifyBlock block -> pure $ Right $ Verified block
    VerifyHeader hdr -> pure $ Right $ Verified hdr


runCardanoConfigs
    :: (IOE :> es, Reader NodeConfig :> es)
    => Eff (Reader ShelleyConfig : Reader (BlockConfig ByronBlock) : es) a
    -> Eff es a
runCardanoConfigs = runByronConfig . runShelleyConfig


runShelleyConfig
    :: (IOE :> es, Reader NodeConfig :> es)
    => Eff (Reader ShelleyConfig : es) a
    -> Eff es a
runShelleyConfig eff = do
    nodeConf <- ask
    shelleyConf <- runExceptT $ readShelleyGenesisConfig nodeConf
    case shelleyConf of
        Left e -> throwIO $ userError $ show e
        Right c -> runReader c eff


runByronConfig
    :: (IOE :> es, Reader NodeConfig :> es)
    => Eff (Reader (BlockConfig ByronBlock) : es) a
    -> Eff es a
runByronConfig eff = do
    nodeConf <- ask
    byronConfig <- runExceptT $ readByronGenesisConfig nodeConf
    case byronConfig of
        Left e -> throwIO $ userError $ show e
        Right c -> runReader (mkByronConfig c (ncByronProtocolVersion nodeConf) Byron.softwareVersion) eff
