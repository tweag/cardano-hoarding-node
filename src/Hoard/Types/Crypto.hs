module Hoard.Types.Crypto (Crypto) where

import Ouroboros.Consensus.Cardano.Block (StandardCrypto)


-- We use StandardCrypto which is the standard cryptographic primitives used in
-- the Cardano mainnet and testnets.
type Crypto = StandardCrypto
