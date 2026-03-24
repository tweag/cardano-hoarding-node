module Hoard.Effects.ChainDB
    ( -- * Effect
      ChainDB

      -- * Handlers
    , runChainDB

      -- * Operations
    , feedBlock
    , getImmutableTip

      -- * Configuration
    , Config (..)
    ) where

import Control.Concurrent.STM (atomically)
import Control.ResourceRegistry (forkThread, withRegistry)
import Control.Tracer (Tracer (..))
import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import Effectful (Effect, IOE, Limit (..), Persistence (..), UnliftStrategy (..), withEffToIO)
import Effectful.Dispatch.Dynamic (interpretWith)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Ouroboros.Consensus.Block (blockPoint, realPointToPoint)
import Ouroboros.Consensus.Ledger.Basics (GetTip (..))
import Ouroboros.Consensus.Node (openChainDB, stdMkChainDbHasFS)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Storage.ChainDB.API (followerForward, followerInstructionBlocking)
import Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment (noPunishment)
import Ouroboros.Consensus.Storage.ChainDB.Impl (defaultArgs)
import Ouroboros.Consensus.Storage.ChainDB.Impl.Args (ChainDbArgs (..), ChainDbSpecificArgs (..))
import Ouroboros.Consensus.Storage.ChainDB.Impl.Types
    ( TraceAddBlockEvent (..)
    , TraceCopyToImmutableDBEvent (..)
    , TraceEvent (..)
    , TraceValidationEvent (..)
    )
import Ouroboros.Consensus.Storage.LedgerDB.Args (LedgerDbFlavorArgs (LedgerDbFlavorArgsV2))
import Ouroboros.Network.Block (ChainUpdate (..), castPoint, genesisPoint)

import Ouroboros.Consensus.Storage.ChainDB.API qualified as CDB
import Ouroboros.Consensus.Storage.LedgerDB.V2.Args qualified as V2

import Atelier.Effects.Publishing (Pub, publish)
import Atelier.Types.QuietSnake (QuietSnake (..))
import Hoard.ChainDB.Events (BlockRejected (..), BlockRolledBack (..), BlockSealed (..), ChainExtended (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoPoint)


data Config = Config
    { enabled :: Bool
    -- ^ Whether the embedded ChainDB is active
    , databaseDirectory :: FilePath
    -- ^ Path to the ChainDB on disk
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def = Config {enabled = False, databaseDirectory = "chaindb"}


data ChainDB :: Effect where
    FeedBlock :: CardanoBlock -> ChainDB m ()
    GetImmutableTip :: ChainDB m CardanoPoint


makeEffect ''ChainDB


runChainDB
    :: ( IOE :> es
       , Pub BlockRejected :> es
       , Pub BlockRolledBack :> es
       , Pub BlockSealed :> es
       , Pub ChainExtended :> es
       , Reader (ProtocolInfo CardanoBlock) :> es
       , Reader Config :> es
       )
    => Eff (ChainDB : es) a
    -> Eff es a
runChainDB action = do
    cfg <- ask @Config
    if not cfg.enabled then interpretWith action \_ -> \case
        FeedBlock _ -> pure ()
        GetImmutableTip -> pure genesisPoint
    else do
        protocolInfo <- ask @(ProtocolInfo CardanoBlock)
        withEffToIO (ConcUnlift Persistent Unlimited) $ \runInIO -> do
            let tracer = Tracer (runInIO . runTracer mkTracer)
                mkHasFS = stdMkChainDbHasFS cfg.databaseDirectory
                flavorArgs = LedgerDbFlavorArgsV2 (V2.V2Args V2.InMemoryHandleArgs)
                customise args = args {cdbsArgs = args.cdbsArgs {cdbsTracer = tracer}}
            withRegistry $ \registry -> do
                (chainDB, _) <-
                    openChainDB
                        registry
                        protocolInfo.pInfoConfig
                        protocolInfo.pInfoInitLedger
                        mkHasFS
                        mkHasFS
                        flavorArgs
                        defaultArgs
                        customise
                _ <- forkThread registry "chaindb-follower" $ withRegistry $ \followerRegistry -> do
                    follower <-
                        CDB.newFollower
                            chainDB
                            followerRegistry
                            CDB.SelectedChain
                            (CDB.GetBlock :: CDB.BlockComponent CardanoBlock CardanoBlock)
                    tip <- atomically $ CDB.getTipPoint chainDB
                    _ <- followerForward follower [tip]
                    forever
                        $ followerInstructionBlocking follower >>= \case
                            AddBlock block -> runInIO $ publish (ChainExtended (blockPoint block))
                            RollBack point -> runInIO $ publish (BlockRolledBack point)
                runInIO $ interpretWith action \_env -> \case
                    FeedBlock block ->
                        liftIO $ CDB.addBlock_ chainDB noPunishment block
                    GetImmutableTip ->
                        liftIO $ atomically $ do
                            ledger <- CDB.getImmutableLedger chainDB
                            pure $ castPoint (getTip ledger)


mkTracer
    :: ( Pub BlockRejected :> es
       , Pub BlockSealed :> es
       )
    => Tracer (Eff es) (TraceEvent CardanoBlock)
mkTracer = Tracer $ \case
    TraceCopyToImmutableDBEvent (CopiedBlockToImmutableDB point) ->
        publish (BlockSealed point)
    TraceCopyToImmutableDBEvent NoBlocksToCopyToImmutableDB ->
        pure ()
    TraceAddBlockEvent e -> case e of
        AddBlockValidation (InvalidBlock err rpoint) ->
            publish (BlockRejected (realPointToPoint rpoint) err)
        AddBlockValidation (ValidCandidate _chain) -> pure ()
        AddBlockValidation (UpdateLedgerDbTraceEvent _event) -> pure ()
        IgnoreBlockOlderThanImmTip _point -> pure ()
        IgnoreBlockAlreadyInVolatileDB _point -> pure ()
        IgnoreInvalidBlock _point _err -> pure ()
        AddedBlockToQueue _point _queueSize -> pure ()
        PoppingFromQueue -> pure ()
        PoppedBlockFromQueue _point -> pure ()
        AddedReprocessLoEBlocksToQueue _queueSize -> pure ()
        PoppedReprocessLoEBlocksFromQueue -> pure ()
        AddedBlockToVolatileDB _point _blockNo _isEBB _enclosing -> pure ()
        TryAddToCurrentChain _point -> pure ()
        TrySwitchToAFork _point _diff -> pure ()
        StoreButDontChange _point -> pure ()
        ChainSelectionLoEDebug _chain _loe -> pure ()
        AddedToCurrentChain _events _info _oldChain _newChain -> pure ()
        SwitchedToAFork _events _info _oldChain _newChain -> pure ()
        PipeliningEvent _event -> pure ()
        ChangingSelection _point -> pure ()
    TraceFollowerEvent _event ->
        pure ()
    TraceGCEvent _event ->
        pure ()
    TraceInitChainSelEvent _event ->
        pure ()
    TraceOpenEvent _event ->
        pure ()
    TraceIteratorEvent _event ->
        pure ()
    TraceLedgerDBEvent _event ->
        pure ()
    TraceImmutableDBEvent _event ->
        pure ()
    TraceVolatileDBEvent _event ->
        pure ()
    TraceLastShutdownUnclean ->
        pure ()
    TraceChainSelStarvationEvent _event ->
        pure ()
