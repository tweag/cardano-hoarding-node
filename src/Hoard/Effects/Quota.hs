{-# LANGUAGE TemplateHaskell #-}

-- | Quota tracking effect for message limits
--
-- Tracks the total number of messages per key, enforcing a hard limit on
-- messages per key over the lifetime of the application.
--
-- This is not traditional rate limiting (messages per time window), but rather
-- a quota system - a fixed capacity limit without time-based recovery.
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
--             FirstOverflow -> do
--                 warn $ "Peer " <> show block.peerId <> " exceeded quota at slot " <> show block.slotNumber
--                 markPeerAsSpam block.peerId
--             SubsequentOverflow ->
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

import Effectful (Eff, Effect, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (interpretWith, localSeqUnlift)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Prelude hiding (Reader, ask)

import Data.HashMap.Strict qualified as HashMap
import Effectful.Concurrent.STM qualified as STM

import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, addEvent)
import Hoard.Effects.Quota.Config


-- | Status of a message with respect to quota limits
data MessageStatus
    = -- | Message is within quota limits
      Accepted
    | -- | First message exceeding the quota
      FirstOverflow
    | -- | Subsequent messages after quota was exceeded
      SubsequentOverflow
    deriving stock (Eq, Show)


-- | Quota tracking effect parameterized by key type
--
-- The key type must be:
-- - 'Ord' and 'Hashable' for efficient storage
-- - 'Show' for tracing/logging
data Quota key :: Effect where
    -- | Execute an action with quota checking
    --
    -- The action receives:
    -- - The current message count for this key (after incrementing)
    -- - The status indicating whether the quota is exceeded
    WithQuotaCheck :: (Hashable key, Ord key, Show key) => key -> (Int -> MessageStatus -> m a) -> Quota key m a


makeEffect ''Quota


-- | Internal state for a quota key
data QuotaState = QuotaState
    { messageCount :: !Int
    , limitExceeded :: !Bool
    }
    deriving stock (Show)


-- | Global quota store: map from keys to per-key state
type QuotaStore key = HashMap.HashMap key (TVar QuotaState)


-- | Run the Quota effect with in-memory STM-based tracking
--
-- Reads quota configuration from the environment.
runQuota
    :: forall key es a
     . (Concurrent :> es, Hashable key, Reader Config :> es, Show key, Tracing :> es)
    => Eff (Quota key : es) a
    -> Eff es a
runQuota action = do
    quotaConfig <- ask @Config
    let maxMessages = quotaConfig.maxBlocksPerPeerPerSlot

    store :: TVar (QuotaStore key) <- STM.newTVarIO HashMap.empty

    interpretWith action $ \env -> \case
        WithQuotaCheck key continuation -> localSeqUnlift env $ \unlift -> do
            (count, status) <- STM.atomically $ checkAndUpdate store maxMessages key

            case status of
                Accepted -> do
                    addAttribute "quota.status" "accepted"
                    addEvent "quota_check" [("key", show key), ("count", show count), ("status", "accepted")]
                FirstOverflow -> do
                    addAttribute "quota.status" "first_overflow"
                    addEvent "quota_exceeded" [("key", show key), ("count", show count), ("limit", show maxMessages)]
                SubsequentOverflow -> do
                    addAttribute "quota.status" "subsequent_overflow"
                    addEvent "quota_overflow_continued" [("key", show key), ("count", show count)]

            unlift $ continuation count status


-- | Atomically check and update quota state for a key
checkAndUpdate
    :: (Hashable key)
    => TVar (QuotaStore key)
    -> Int
    -> key
    -> STM (Int, MessageStatus)
checkAndUpdate storeVar maxMessages key = do
    store <- STM.readTVar storeVar

    stateVar <- case HashMap.lookup key store of
        Just var -> pure var
        Nothing -> do
            var <- STM.newTVar QuotaState {messageCount = 0, limitExceeded = False}
            STM.writeTVar storeVar (HashMap.insert key var store)
            pure var

    currentState <- STM.readTVar stateVar
    let newCount = currentState.messageCount + 1
        status
            | newCount <= maxMessages = Accepted
            | currentState.limitExceeded = SubsequentOverflow
            | otherwise = FirstOverflow

        newState =
            QuotaState
                { messageCount = newCount
                , limitExceeded = status /= Accepted
                }

    STM.writeTVar stateVar newState
    pure (newCount, status)
