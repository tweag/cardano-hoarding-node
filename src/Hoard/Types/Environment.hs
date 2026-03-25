module Hoard.Types.Environment
    ( -- * peer-snapshot.json
      PeerSnapshotFile (..)
    , LedgerPool (..)
    , BootstrapPeerIP (..)
    , BootstrapPeerDomain (..)

      -- * Topology
    , Topology (..)
    )
where

import Data.Aeson (FromJSON (..), withObject, (.:))
import GHC.Generics (Generically (Generically))


data Topology = Topology
    { peerSnapshotFile :: FilePath
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via Generically Topology


data PeerSnapshotFile = PeerSnapshotFile
    { bigLedgerPools :: [LedgerPool]
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via Generically PeerSnapshotFile


data LedgerPool = LedgerPool
    { relays :: [Either BootstrapPeerDomain BootstrapPeerIP]
    }
    deriving stock (Eq, Generic, Show)


-- Custom FromJSON instance for LedgerPool to handle Either in relays
instance FromJSON LedgerPool where
    parseJSON = withObject "LedgerPool" $ \o -> do
        relaysArray <- o .: "relays"
        relays <- forM relaysArray $ \relayValue ->
            -- Try parsing as domain first, then as IP
            (Left <$> parseJSON @BootstrapPeerDomain relayValue)
                <|> (Right <$> parseJSON @BootstrapPeerIP relayValue)
        pure $ LedgerPool {relays}


data BootstrapPeerIP = BootstrapPeerIP
    { address :: Text
    , port :: Int
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via Generically BootstrapPeerIP


data BootstrapPeerDomain = BootstrapPeerDomain
    { domain :: Text
    , port :: Int
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via Generically BootstrapPeerDomain
