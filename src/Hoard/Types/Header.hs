module Hoard.Types.Header
    ( Header
    )
where

import Ouroboros.Consensus.Cardano.Block qualified as Block

import Hoard.Types.Cardano (Crypto)


type Header = Block.Header Crypto
