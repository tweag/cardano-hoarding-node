module Hoard.Types.Cardano
    ( Crypto
    , CardanoBlock
    , CardanoHeader
    , CardanoPoint
    , CardanoTip
    , CardanoCodecs
    ) where

import Cardano.Api ()
import Codec.CBOR.Read (DeserialiseFailure)
import Data.ByteString.Lazy qualified as LBS
import Network.Socket (SockAddr)
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import Ouroboros.Network.Block qualified as Network
import Prelude hiding (Reader, State, asks, evalState, gets)


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
