-- | Quota tracking effect for message limits
--
-- Tracks the total number of messages per key, enforcing a hard limit on
-- messages per key with time-based eviction.
--
-- Quota entries are evicted after a configurable TTL (time-to-live), allowing
-- the cache to naturally clean up old entries and prevent unbounded memory growth.
--
-- The effect is polymorphic over the key type, allowing it to be used for
-- various tracking scenarios (e.g., per-peer-per-slot for blocks).
--
-- == Basic Usage
--
-- @
-- handleBlock :: (Quota (ID Peer, Int64) :> es) => Block -> Eff es ()
-- handleBlock block = do
--     let key = (block.peerId, block.slotNumber)
--     withQuotaCheck key $ \\count status -> do
--         case status of
--             Accepted -> insertBlock block
--             Overflow 1 -> do
--                 warn $ "Peer " <> show block.peerId <> " exceeded quota at slot " <> show block.slotNumber
--                 markPeerAsSpam block.peerId
--             Overflow _ ->
--                 debug $ "Additional spam from " <> show block.peerId <> " (count: " <> show count <> ")"
-- @
module Hoard.Effects.Quota
    ( -- * Effect
      Quota
    , MessageStatus (..)
    , withQuotaCheck

      -- * Re-exports
    , module Hoard.Effects.Quota.Config

      -- * Interpreters
    , runQuota
    ) where

import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Effectful (Eff, Effect, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Dispatch.Dynamic (interpretWith, localSeqUnlift)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import StmContainers.Map (Map)
import Prelude hiding (Map, Reader, ask)

import Effectful.Concurrent.STM qualified as STM
import ListT qualified
import StmContainers.Map qualified as Map

import Hoard.Effects.Clock (Clock, currentTime)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Monitoring.Tracing (Attr (..), ToAttribute, Tracing, addAttribute, addEvent, withSpan)
import Hoard.Effects.Quota.Config

import Hoard.Effects.Conc qualified as Conc


-- | Status of a message with respect to quota limits
--
-- The 'Int' in 'Overflow' is a count of how many times this key has
-- exceeded its quota, so callers can pattern match on @Overflow 1@ to react
-- specifically to the first violation without comparing counts manually.
data MessageStatus
    = -- | Message is within quota limits
      Accepted
    | -- | Message exceeds quota; the 'Int' is the number of overflows so far
      Overflow Int
    deriving stock (Eq, Show)


-- | Quota tracking effect parameterized by key type
data Quota key :: Effect where
    -- | Execute an action with quota checking
    --
    -- The action receives:
    -- - The current message count for this key (after incrementing)
    -- - The status indicating whether the quota is exceeded
    WithQuotaCheck
        :: (Hashable key, Ord key)
        => key -> (Int -> MessageStatus -> m a) -> Quota key m a


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
     . (Clock :> es, Conc :> es, Concurrent :> es, Hashable key, Reader Config :> es, ToAttribute key, Tracing :> es)
    => Int
    -- ^ Maximum number of messages allowed per key
    -> Eff (Quota key : es) a
    -> Eff es a
runQuota maxMessages action = do
    quotaConfig <- ask @Config
    let ttl = quotaConfig.entryTtl
        intervalMicros = round (realToFrac quotaConfig.cleanupInterval * 1_000_000 :: Double)

    store <- STM.atomically Map.new

    Conc.fork_ $ forever $ do
        threadDelay intervalMicros
        now <- currentTime
        evicted <- STM.atomically $ evictExpiredEntries store ttl now
        when (evicted > 0)
            $ addEvent "quota_cleanup" [("evicted.count", evicted)]

    interpretWith action $ \env -> \case
        WithQuotaCheck key continuation -> localSeqUnlift env $ \unlift ->
            withSpan "quota_check" $ do
                now <- currentTime
                (count, status) <- STM.atomically $ checkAndUpdate store maxMessages now key

                case status of
                    Accepted -> do
                        addAttribute @Text "quota.status" "accepted"
                        addEvent "quota_check" [("key", Attr key), ("count", Attr count), ("status", Attr @Text "accepted")]
                    Overflow n -> do
                        addAttribute @Text "quota.status" "overflow"
                        addEvent "quota_exceeded" [("key", Attr key), ("overflow_count", Attr n), ("limit", Attr maxMessages)]

                unlift $ continuation count status


-- | Atomically check and update quota state for a key
checkAndUpdate
    :: (Hashable key)
    => Map key QuotaState
    -> Int
    -> UTCTime
    -> key
    -> STM (Int, MessageStatus)
checkAndUpdate store maxMessages now key = do
    mst <- Map.lookup key store

    let newState = case mst of
            Nothing -> QuotaState {messageCount = 1, createdAt = now}
            Just st -> st {messageCount = st.messageCount + 1}

        newCount = newState.messageCount

        status
            | newCount <= maxMessages = Accepted
            | otherwise = Overflow (newCount - maxMessages)

    Map.insert newState key store
    pure (newCount, status)


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
