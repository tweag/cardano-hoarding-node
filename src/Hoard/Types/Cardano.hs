module Hoard.Types.Cardano
    ( Crypto
    , CardanoBlock
    , CardanoHeader
    , CardanoPoint
    , CardanoTip
    , CardanoCodecs
    , CardanoMiniProtocol
    , ChainPoint (..)
    ) where

import Codec.CBOR.Read (DeserialiseFailure)
import Data.Aeson (FromJSON, ToJSON)
import Network.Mux (Mode (..))
import Network.Socket (SockAddr)
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Context (MinimalInitiatorContext, ResponderContext)
import Ouroboros.Network.Mux (MiniProtocol)
import Rel8 (DBType, JSONBEncoded (JSONBEncoded))

import Cardano.Api qualified as C
import Data.ByteString.Lazy qualified as LBS
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Network.Block qualified as Network


-- We use StandardCrypto which is the standard cryptographic primitives used in
-- the Cardano mainnet and testnets.
type Crypto = Consensus.StandardCrypto


-- | Type aliases for Cardano block types used throughout the network events.
--
-- These use StandardCrypto which is the standard cryptographic primitives
-- used in the Cardano mainnet and testnets.
type CardanoBlock = Consensus.CardanoBlock Crypto


type CardanoHeader = Consensus.Header CardanoBlock


type CardanoPoint = Network.Point CardanoBlock


type CardanoTip = Network.Tip CardanoBlock


type CardanoCodecs =
    Codecs
        CardanoBlock
        SockAddr
        DeserialiseFailure
        IO
        LBS.ByteString
        LBS.ByteString
        LBS.ByteString
        LBS.ByteString
        LBS.ByteString
        LBS.ByteString
        LBS.ByteString


type CardanoMiniProtocol =
    MiniProtocol
        'InitiatorMode
        (MinimalInitiatorContext SockAddr)
        (ResponderContext SockAddr)
        LBS.ByteString
        IO
        ()
        Void


newtype ChainPoint = ChainPoint C.ChainPoint
    deriving (Show)
    deriving (Eq, FromJSON, Ord, ToJSON) via C.ChainPoint
    deriving (DBType) via JSONBEncoded ChainPoint
