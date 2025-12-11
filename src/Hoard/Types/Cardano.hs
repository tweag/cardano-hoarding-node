module Hoard.Types.Cardano
    ( Crypto
    , CardanoBlock
    , CardanoHeader
    , CardanoPoint
    , CardanoTip
    ) where

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
