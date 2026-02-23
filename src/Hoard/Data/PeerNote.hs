module Hoard.Data.PeerNote
    ( PeerNote (..)
    , NoteType (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Rel8 (DBEq, DBType, ReadShow (..))

import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)
import Hoard.Types.JsonReadShow (JsonReadShow (..))


-- | A given note about a peer.
data PeerNote = PeerNote
    { id :: ID PeerNote
    , peerId :: ID Peer
    , noteType :: NoteType
    , note :: Text
    , notedAt :: UTCTime
    }


-- | Potential forms of notes about peers.
data NoteType
    = Adversarial
    deriving (Eq, Generic, Ord)
    deriving (Read, Show)
    deriving (FromJSON, ToJSON) via JsonReadShow NoteType
    deriving (DBEq, DBType) via ReadShow NoteType
