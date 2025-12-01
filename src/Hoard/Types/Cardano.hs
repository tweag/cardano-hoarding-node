module Hoard.Types.Cardano
    ( Crypto
    , CardanoBlock
    , CardanoHeader
    , CardanoPoint
    , CardanoTip
    ) where

import Ouroboros.Consensus.Cardano.Block qualified as Block
import Ouroboros.Network.Block qualified as Block


-- We use StandardCrypto which is the standard cryptographic primitives used in
-- the Cardano mainnet and testnets.
type Crypto = Block.StandardCrypto


-- | Type aliases for Cardano block types used throughout the network events.
--
-- These use StandardCrypto which is the standard cryptographic primitives
-- used in the Cardano mainnet and testnets.
type CardanoBlock = Block.CardanoBlock Crypto


type CardanoHeader = Block.Header CardanoBlock


type CardanoPoint = Block.Point CardanoBlock


type CardanoTip = Block.Tip CardanoBlock
