module Hoard.Data.ProtocolInfo
    ( CardanoProtocolInfo
    , ProtocolConfigPath (..)
    , loadProtocolInfo
    , loadNodeConfig
    ) where

import Cardano.Api
    ( File (..)
    , NodeConfig
    , mkProtocolInfoCardano
    , readCardanoGenesisConfig
    , readNodeConfig
    )
import Effectful (Eff, IOE, (:>))
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))

import Hoard.Types.Cardano (CardanoBlock)


type CardanoProtocolInfo = ProtocolInfo CardanoBlock


newtype ProtocolConfigPath = ProtocolConfigPath {getProtocolConfigPath :: FilePath}


loadNodeConfig :: (IOE :> es) => FilePath -> Eff es NodeConfig
loadNodeConfig configPath = do
    let configFile = File configPath
    nodeConfigResult <- runExceptT $ readNodeConfig configFile
    case nodeConfigResult of
        Left err -> error $ "Failed to read node config: " <> err
        Right cfg -> pure cfg


-- | Load the Cardano protocol info from config files.
-- This is needed to get the CodecConfig for creating proper codecs.
loadProtocolInfo :: (IOE :> es) => ProtocolConfigPath -> Eff es (ProtocolInfo CardanoBlock)
loadProtocolInfo (ProtocolConfigPath configPath) = do
    -- Load NodeConfig
    nodeConfig <- loadNodeConfig configPath

    -- Load GenesisConfig
    genesisConfigResult <- runExceptT $ readCardanoGenesisConfig nodeConfig
    genesisConfig <- case genesisConfigResult of
        Left err -> error $ "Failed to read genesis config: " <> show err
        Right cfg -> pure cfg

    -- Create ProtocolInfo
    let (protocolInfo, _mkBlockForging) = mkProtocolInfoCardano genesisConfig
    pure protocolInfo
