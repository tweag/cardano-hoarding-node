-- | Quota tracking effect for message limits
--
-- Tracks the total number of messages per key with time-based eviction.
--
-- Quota entries are evicted after a configurable TTL (time-to-live), allowing
-- the cache to naturally clean up old entries and prevent unbounded memory growth.
--
-- The effect is polymorphic over both the key type and the status type returned
-- by a caller-supplied classifier function. This allows callers to define their
-- own thresholds and result types rather than relying on a single hard limit.
--
-- == Basic Usage
--
-- @
-- checkDuplicate :: (Quota DuplicateKey :> es) => DuplicateKey -> Config -> Eff es ()
-- checkDuplicate key cfg =
--     withQuotaCheck key classify $ \case
--         Nothing      -> pure ()
--         Just Minor   -> warnAboutPeer
--         Just Critical -> banPeer
--   where
--     classify c
--         | c > cfg.criticalThreshold = Just Critical
--         | c > cfg.warningThreshold  = Just Minor
--         | otherwise                 = Nothing
-- @
module Atelier.Effects.Quota
    ( -- * Effect
      Quota
    , withQuotaCheck

      -- * Re-exports
    , module Atelier.Effects.Quota.Config

      -- * Interpreters
    , runQuota
    , runQuotaConst
    ) where

import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Effectful (Effect)
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (STM)
import Effectful.Dispatch.Dynamic (interpret, interpretWith, localSeqUnlift)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import StmContainers.Map (Map)

import Effectful.Concurrent.STM qualified as STM
import ListT qualified
import StmContainers.Map qualified as Map

import Atelier.Effects.Clock (Clock, currentTime)
import Atelier.Effects.Conc (Conc)
import Atelier.Effects.Delay (Delay, Microsecond)
import Atelier.Effects.Log (Log)
import Atelier.Effects.Quota.Config
import Prelude hiding (Map)

import Atelier.Effects.Conc qualified as Conc
import Atelier.Effects.Delay qualified as Delay
import Atelier.Effects.Log qualified as Log


-- | Quota tracking effect parameterized by key type
data Quota key :: Effect where
    -- | Increment the hit count for a key and run an action based on a classifier.
    --
    -- The classifier receives the new count and produces a status value of any
    -- type the caller chooses. The continuation then receives that status.
    WithQuotaCheck
        :: (Hashable key, Ord key)
        => key -> (Int -> status) -> (status -> m a) -> Quota key m a


makeEffect ''Quota


-- | Internal state for a quota key
data QuotaState = QuotaState
    { messageCount :: !Int
    , createdAt :: !UTCTime
    }
    deriving stock (Show)


-- | Run the Quota effect with in-memory STM-based tracking
--
-- Reads quota configuration from the environment.
-- A background thread periodically evicts expired entries based on TTL.
runQuota
    :: forall key es a
     . ( Clock :> es
       , Conc :> es
       , Concurrent :> es
       , Delay :> es
       , Hashable key
       , Log :> es
       , Reader Config :> es
       )
    => Eff (Quota key : es) a
    -> Eff es a
runQuota action = do
    quotaConfig <- ask @Config
    let ttl = quotaConfig.entryTtl

    store <- STM.atomically Map.new

    Conc.fork_ $ forever $ Log.withNamespace "Quota" do
        Delay.wait $ Delay.nominalDiffTime @Microsecond quotaConfig.cleanupInterval
        now <- currentTime
        evicted <- STM.atomically $ evictExpiredEntries store ttl now

        when (evicted > 0)
            $ Log.debug
            $ "Evicted " <> show evicted <> " entries"

    interpretWith action $ \env -> \case
        WithQuotaCheck key classify continuation -> localSeqUnlift env $ \unlift -> do
            now <- currentTime
            count <- STM.atomically $ incrementAndGet store now key
            unlift $ continuation (classify count)


-- | Atomically increment the hit count for a key and return the new count
incrementAndGet
    :: (Hashable key)
    => Map key QuotaState
    -> UTCTime
    -> key
    -> STM Int
incrementAndGet store now key = do
    mst <- Map.lookup key store

    let newState = case mst of
            Nothing -> QuotaState {messageCount = 1, createdAt = now}
            Just st -> st {messageCount = st.messageCount + 1}

    Map.insert newState key store
    pure newState.messageCount


-- | Remove all entries whose TTL has expired. Returns the number of evicted entries.
evictExpiredEntries
    :: (Hashable key)
    => Map key QuotaState
    -> NominalDiffTime
    -> UTCTime
    -> STM Int
evictExpiredEntries store ttl now = do
    ListT.fold
        ( \count (k, v) ->
            if now >= addUTCTime ttl v.createdAt then
                Map.delete k store $> count + 1
            else
                pure count
        )
        0
        $ Map.listT store


runQuotaConst :: Int -> Eff (Quota key : es) a -> Eff es a
runQuotaConst c = interpret \env -> \case
    WithQuotaCheck _ classify f -> localSeqUnlift env \unlift -> unlift $ f (classify c)
