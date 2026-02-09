module Hoard.Effects.Verifier
    ( -- * Effect
      Verifier (..)
    , verifyCardanoHeader
    , verifyCardanoBlock
    , verifyBlock
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
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Exception (throwIO)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.TH (makeEffect)
import Ouroboros.Consensus.Block (BlockConfig)
import Ouroboros.Consensus.Byron.Node (mkByronConfig)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..), Header (..))
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, ShelleyCompatible, shelleyHeaderRaw)
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtocolHeaderSupportsKES)
import System.IO.Error (userError)
import Prelude hiding (Reader, ask, runReader)

import Cardano.Api.Byron qualified as Byron
import Ouroboros.Consensus.Byron.Ledger.Integrity qualified as Byron
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley

import Hoard.Data.Block (Block (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)


data Verifier :: Effect where
    -- | Verify that the header signature is correct and valid.
    VerifyCardanoHeader :: CardanoHeader -> Verifier m (VerificationResult CardanoHeader)
    VerifyCardanoBlock :: CardanoBlock -> Verifier m (VerificationResult CardanoBlock)
    VerifyBlock :: Block -> Verifier m (VerificationResult Block)


type VerificationResult a = Either (Verified 'Invalid a) (Verified 'Valid a)


data Validity = Valid | Invalid


newtype Verified (v :: Validity) a = Verified a


getVerified :: Verified v a -> a
getVerified (Verified a) = a


makeEffect ''Verifier


runVerifier
    :: forall es a
     . (Reader (BlockConfig ByronBlock) :> es, Reader ShelleyConfig :> es)
    => Eff (Verifier : es) a
    -> Eff es a
runVerifier = interpret_ \case
    VerifyCardanoHeader header -> do
        let verifyShelley :: (ProtocolHeaderSupportsKES proto) => Header (ShelleyBlock proto era) -> Eff es (VerificationResult CardanoHeader)
            verifyShelley h = do
                shelleyConf <- ask @ShelleyConfig
                let spKES = shelleyConf.scConfig.sgSlotsPerKESPeriod
                if Shelley.verifyHeaderIntegrity spKES (shelleyHeaderRaw h) then
                    pure $ Right $ Verified header
                else
                    pure $ Left $ Verified header
        case header of
            HeaderByron h -> do
                byronConf <- ask @(BlockConfig ByronBlock)
                if Byron.verifyHeaderIntegrity byronConf h then
                    pure $ Right $ Verified header
                else
                    pure $ Left $ Verified header
            HeaderShelley h -> verifyShelley h
            HeaderAllegra h -> verifyShelley h
            HeaderConway h -> verifyShelley h
            HeaderBabbage h -> verifyShelley h
            HeaderMary h -> verifyShelley h
            HeaderAlonzo h -> verifyShelley h
            HeaderDijkstra h -> verifyShelley h
    VerifyCardanoBlock block -> verifyCardanoBlock' block
    VerifyBlock block -> do
        bimap (const $ Verified block) (const $ Verified block) <$> verifyCardanoBlock' block.blockData
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
            BlockByron b -> do
                byronConf <- ask @(BlockConfig ByronBlock)
                if Byron.verifyBlockIntegrity byronConf b then
                    pure $ Right $ Verified block
                else
                    pure $ Left $ Verified block
            BlockShelley b -> verifyShelley b
            BlockAllegra b -> verifyShelley b
            BlockConway b -> verifyShelley b
            BlockBabbage b -> verifyShelley b
            BlockMary b -> verifyShelley b
            BlockAlonzo b -> verifyShelley b
            BlockDijkstra b -> verifyShelley b


runAllValidVerifier :: Eff (Verifier : es) a -> Eff es a
runAllValidVerifier = interpret_ \case
    VerifyCardanoHeader header -> pure $ Right $ Verified header
    VerifyCardanoBlock block -> pure $ Right $ Verified block
    VerifyBlock block -> pure $ Right $ Verified block


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
