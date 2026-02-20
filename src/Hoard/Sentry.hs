module Hoard.Sentry
    ( component
    , DuplicateBlockKey (..)
    , PeerMarkedAsMalicious (..)
    ) where

import Data.Time (UTCTime)
import Effectful (Eff, (:>))
import Ouroboros.Consensus.Block (getHeader)

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Data.BlockHash (BlockHash, blockHashFromHeader)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Monitoring.Tracing
    ( SpanStatus (..)
    , ToAttribute
    , ToAttributeShow (..)
    , Tracing
    , addAttribute
    , setStatus
    , withSpan
    )
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Quota (MessageStatus (..), Quota, withQuotaCheck)
import Hoard.Events.BlockFetch (BlockReceived (..))

import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Publishing qualified as Sub


component
    :: ( Clock :> es
       , Pub PeerMarkedAsMalicious :> es
       , Quota DuplicateBlockKey :> es
       , Sub BlockReceived :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "Sentry"
        , listeners =
            pure
                [ Sub.listen duplicateBlockGuard
                ]
        }


duplicateBlockGuard
    :: ( Clock.Clock :> es
       , Pub PeerMarkedAsMalicious :> es
       , Quota DuplicateBlockKey :> es
       , Tracing :> es
       )
    => BlockReceived -> Eff es ()
duplicateBlockGuard event = withSpan "sentry.duplicate_block_guard" do
    let blockHash = blockHashFromHeader $ getHeader event.block
    addAttribute "peer.id" event.peer.id
    addAttribute "peer.address" event.peer.address
    addAttribute "block.hash" blockHash

    withQuotaCheck (DuplicateBlockKey event.peer.id blockHash) \_ -> \case
        Overflow 1 -> do
            setStatus $ Error "Duplicate block threshold exceeded"
            timestamp <- Clock.currentTime
            publish
                $ PeerMarkedAsMalicious
                    { timestamp
                    , peer = event.peer
                    , reason = "Duplicate block threshold exceeded"
                    }
        Overflow n -> do
            setStatus $ Error "Duplicate block threshold keeps being exceeded"
            addAttribute "times" n
            addAttribute "already_marked_as_malicious" True
        _ ->
            setStatus Ok


data DuplicateBlockKey = DuplicateBlockKey {id :: (ID Peer), hash :: BlockHash}
    deriving (Hashable)
    deriving stock (Eq, Generic, Ord, Show)
    deriving (ToAttribute) via ToAttributeShow DuplicateBlockKey


data PeerMarkedAsMalicious = PeerMarkedAsMalicious
    { timestamp :: UTCTime
    , peer :: Peer
    , reason :: Text
    }
    deriving (Show, Typeable)
