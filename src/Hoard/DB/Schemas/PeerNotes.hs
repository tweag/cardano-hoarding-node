module Hoard.DB.Schemas.PeerNotes
    ( Row (..)
    , schema
    , peerNoteFromRow
    , rowFromPeerNote
    )
where

import Data.Time (UTCTime)
import Rel8
    ( Column
    , Expr
    , Name
    , Rel8able
    , Result
    , TableSchema
    , lit
    )

import Hoard.DB.Schema (mkSchema)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Data.PeerNote (NoteType, PeerNote (..))


data Row f = Row
    { id :: Column f (ID PeerNote)
    , peerId :: Column f (ID Peer)
    , noteType :: Column f NoteType
    , note :: Column f Text
    , notedAt :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)


deriving instance Eq (Row Result)


deriving instance Show (Row Result)


schema :: TableSchema (Row Name)
schema = mkSchema "peer_notes"


peerNoteFromRow :: Row Result -> PeerNote
peerNoteFromRow row =
    PeerNote
        { id = row.id
        , peerId = row.peerId
        , noteType = row.noteType
        , note = row.note
        , notedAt = row.notedAt
        }


-- | Convert a Peer domain type to a database row
rowFromPeerNote :: PeerNote -> Row Expr
rowFromPeerNote note =
    Row
        { id = lit note.id
        , peerId = lit note.peerId
        , noteType = lit note.noteType
        , note = lit note.note
        , notedAt = lit note.notedAt
        }
