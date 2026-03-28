module Integration.Hoard.DB.PeerNotePersistenceSpec (spec_PeerNotePersistence) where

import Data.Time (getCurrentTime)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Test.Hspec
import Text.Read (read)

import Atelier.Effects.Clock (runClock)
import Atelier.Effects.Monitoring.Metrics (runMetricsNoOp)
import Atelier.Effects.Monitoring.Tracing (runTracingNoOp)
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Data.PeerNote (NoteType (..), PeerNote (..))
import Hoard.Effects.DB (runDBRead, runDBWrite, runRel8Read, runRel8Write)
import Hoard.Effects.PeerNoteRepo (runPeerNoteRepo, saveNote)
import Hoard.Effects.PeerRepo (getPeerByAddress, runPeerRepo, upsertPeers)
import Hoard.TestHelpers.Database (withCleanTestDatabase)

import Atelier.Effects.Log qualified as Log


spec_PeerNotePersistence :: Spec
spec_PeerNotePersistence = do
    let run pools action =
            runEff
                . Log.runLogNoOp
                . runErrorNoCallStack @Text
                . runReader pools
                . runMetricsNoOp
                . runTracingNoOp
                . runClock
                . runDBRead
                . runClock
                . runDBWrite
                . runRel8Read
                . runRel8Write
                . runPeerRepo
                . runPeerNoteRepo
                $ action

    withCleanTestDatabase $ describe "PeerNote persistence (database)" $ do
        it "saves a note and returns it with correct fields" $ \pools -> do
            now <- getCurrentTime
            let peerAddr = PeerAddress (read "10.0.0.1") 3001

            Right _ <- run pools $ upsertPeers (fromList [peerAddr]) peerAddr now
            Right (Just peer) <- run pools $ getPeerByAddress peerAddr

            result <- run pools $ saveNote peer.id Adversarial "suspicious activity"
            case result of
                Right peerNote -> do
                    peerNote.peerId `shouldBe` peer.id
                    peerNote.noteType `shouldBe` Adversarial
                    peerNote.note `shouldBe` "suspicious activity"
                Left err -> expectationFailure $ show err
