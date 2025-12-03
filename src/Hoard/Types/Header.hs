module Hoard.Types.Header
    ( Header
    )
where

import Hoard.Types.Cardano (Crypto)
import Ouroboros.Consensus.Cardano.Block qualified as Block


type Header = Block.Header Crypto
