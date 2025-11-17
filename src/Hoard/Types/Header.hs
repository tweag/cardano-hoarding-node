module Hoard.Types.Header
    ( CardanoHeader
    , Header (..)
    , CardanoPoint
    )
where

import Hoard.Types.Cardano (Crypto)
import Ouroboros.Consensus.Block (ConvertRawHash (..), HasHeader, blockHash)
import Ouroboros.Consensus.Block qualified as Block
import Ouroboros.Consensus.Block.Abstract (GetHeader (..))
import Ouroboros.Consensus.Block.Abstract qualified as Header
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Ouroboros.Consensus.Cardano.Block qualified as Block
import Ouroboros.Consensus.HardFork.Combinator.Embed.Unary (Isomorphic (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import Ouroboros.Network.Block (SlotNo)

import Hoard.Data.Peer (PeerAddress)
import Hoard.Types.Cardano (Crypto, CardanoHeader, CardanoBlock, CardanoPoint)
import Hoard.Types.DBHash (DBHash (..), HashKind (..))



data Header = Header
    { headerHash :: DBHash HashForHeader
    -- ^ Hash of the header record itself
    , blockHash :: DBHash HashForBlock
    -- ^ Hash of the body this header represents
    , headerData :: CardanoHeader
    -- ^ Full header value
    }

mkHeader :: CardanoHeader -> Header
mkHeader header = Header
    { headerHash = 
    , slotNo = blockSlot header
    }

blockHashFromHeader :: CardanoHeader -> DBHash HashForBlock
blockHashFromHeader hdr = DBHash $ toRawHash (Proxy :: Proxy CardanoBlock) (Block.blockHash hdr)

toPoint :: Header -> CardanoPoint
toPoint = headerData
