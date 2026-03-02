module Hoard.Effects.PeerNoteRepo
    ( PeerNoteRepo (..)
    , saveNote
    , runPeerNoteRepo
    , runPeerNoteRepoState
    ) where

import Effectful (Effect)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.State.Static.Shared (State, modify)
import Effectful.TH (makeEffect)
import Rel8 (lit)

import Data.UUID qualified as UUID
import Hasql.Transaction qualified as TX
import Rel8 qualified
import Rel8.Expr.Time qualified

import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer)
import Hoard.Data.PeerNote (NoteType, PeerNote (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.DBWrite (DBWrite, runTransaction)

import Hoard.DB.Schemas.PeerNotes qualified as PeerNotes
import Hoard.Effects.Clock qualified as Clock


data PeerNoteRepo :: Effect where
    SaveNote :: ID Peer -> NoteType -> Text -> PeerNoteRepo m PeerNote


makeEffect ''PeerNoteRepo


runPeerNoteRepo :: (DBWrite :> es) => Eff (PeerNoteRepo : es) a -> Eff es a
runPeerNoteRepo = interpret_ \case
    SaveNote peerId noteType note -> do
        runTransaction "save-note"
            . fmap PeerNotes.peerNoteFromRow
            . TX.statement ()
            . Rel8.run1
            $ Rel8.insert
                Rel8.Insert
                    { into = PeerNotes.schema
                    , rows =
                        Rel8.values
                            [ PeerNotes.Row
                                { PeerNotes.id = Rel8.unsafeDefault
                                , PeerNotes.peerId = lit peerId
                                , PeerNotes.noteType = lit noteType
                                , PeerNotes.note = lit note
                                , PeerNotes.notedAt = Rel8.Expr.Time.now
                                }
                            ]
                    , onConflict = Rel8.Abort
                    , returning = Rel8.Returning Prelude.id
                    }


runPeerNoteRepoState :: (Clock :> es, State [PeerNote] :> es) => Eff (PeerNoteRepo : es) a -> Eff es a
runPeerNoteRepoState = interpret_ \case
    SaveNote peerId noteType note -> do
        notedAt <- Clock.currentTime
        let peerNote =
                PeerNote
                    { id = ID $ UUID.fromWords64 0 0
                    , peerId
                    , noteType
                    , note
                    , notedAt
                    }
        modify (peerNote :)
        pure peerNote
