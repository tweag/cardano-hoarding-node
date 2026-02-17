module Unit.Hoard.Effects.QuotaSpec (spec_Quota) where

import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Concurrent.Async (mapConcurrently)
import Effectful.Reader.Static (Reader, runReader)
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (Reader, runReader)

import Data.IORef qualified as IORef

import Hoard.Effects.Clock (Clock, runClock)
import Hoard.Effects.Conc (Conc, runConc)
import Hoard.Effects.Monitoring.Tracing (Tracing, runTracingNoOp)
import Hoard.Effects.Quota (Config (..), MessageStatus (..), Quota, runQuota, withQuotaCheck)


spec_Quota :: Spec
spec_Quota = do
    describe "Basic Behaviors" do
        it "count increments for each check" do
            result <- runQuotaTest 5 $ do
                counts <- traverse (const checkKey) [1 .. 3 :: Int]
                pure counts
            result `shouldBe` [1, 2, 3]

        it "messages within quota are Accepted" do
            result <- runQuotaTest 3 $ do
                statuses <- traverse (const checkStatus) [1 .. 3 :: Int]
                pure statuses
            result `shouldBe` [Accepted, Accepted, Accepted]

        it "first message exceeding quota is FirstOverflow" do
            result <- runQuotaTest 2 $ do
                statuses <- traverse (const checkStatus) [1 .. 3 :: Int]
                pure statuses
            result `shouldBe` [Accepted, Accepted, FirstOverflow]

        it "subsequent messages after FirstOverflow are SubsequentOverflow" do
            result <- runQuotaTest 2 $ do
                statuses <- traverse (const checkStatus) [1 .. 5 :: Int]
                pure statuses
            result `shouldBe` [Accepted, Accepted, FirstOverflow, SubsequentOverflow, SubsequentOverflow]

        it "count is accurate after multiple checks" do
            result <- runQuotaTest 3 $ do
                _ <- traverse (const checkStatus) [1 .. 5 :: Int]
                finalCount <- checkKey
                pure finalCount
            result `shouldBe` 6

    describe "Multiple Keys" do
        it "different keys have independent quotas" do
            result <- runQuotaTest 2 $ do
                key1Statuses <- traverse (const $ checkStatusForKey @Int 1) [1 .. 3 :: Int]
                key2Statuses <- traverse (const $ checkStatusForKey @Int 2) [1 .. 3 :: Int]
                pure (key1Statuses, key2Statuses)
            result
                `shouldBe` ( [Accepted, Accepted, FirstOverflow]
                           , [Accepted, Accepted, FirstOverflow]
                           )

        it "one key at quota doesn't affect other keys" do
            result <- runQuotaTest 2 $ do
                _ <- traverse (const $ checkStatusForKey @Int 1) [1 .. 3 :: Int]
                key2Status <- checkStatusForKey @Int 2
                pure key2Status
            result `shouldBe` Accepted

        it "works with integer keys" do
            result <- runQuotaTest 2 $ do
                key1Status <- checkStatusForKey @Int 1
                key2Status <- checkStatusForKey @Int 42
                key3Status <- checkStatusForKey @Int 999
                pure (key1Status, key2Status, key3Status)
            result `shouldBe` (Accepted, Accepted, Accepted)

    describe "Concurrent Access" do
        it "concurrent checks on same key count all messages" do
            result <- runQuotaTest 100 $ do
                -- Run 50 concurrent checks
                _ <- mapConcurrently (const checkKey) [1 .. 50 :: Int]
                finalCount <- checkKey
                pure finalCount
            result `shouldBe` 51

        it "exactly one thread gets FirstOverflow" do
            result <- runQuotaTest 5 $ do
                -- Run 10 concurrent checks (5 within quota, 5 overflow)
                statuses <- mapConcurrently (const checkStatus) [1 .. 10 :: Int]
                let firstOverflowCount = length $ filter (== FirstOverflow) statuses
                pure firstOverflowCount
            result `shouldBe` 1

        it "concurrent access to different keys doesn't interfere" do
            result <- runQuotaTest 2 $ do
                results <-
                    mapConcurrently
                        (\i -> traverse (const $ checkStatusForKey @Int i) [1 .. 3 :: Int])
                        [1 .. 5 :: Int]
                pure $ fmap (filter (== FirstOverflow)) results
            all (\overflows -> length overflows == 1) result `shouldBe` True

    describe "Edge Cases" do
        it "quota of 0 makes every message overflow" do
            result <- runQuotaTest 0 $ do
                statuses <- traverse (const checkStatus) [1 .. 3 :: Int]
                pure statuses
            result `shouldBe` [FirstOverflow, SubsequentOverflow, SubsequentOverflow]

        it "quota of 1 allows one message then overflows" do
            result <- runQuotaTest 1 $ do
                statuses <- traverse (const checkStatus) [1 .. 3 :: Int]
                pure statuses
            result `shouldBe` [Accepted, FirstOverflow, SubsequentOverflow]

        it "large quota values work correctly" do
            result <- runQuotaTest 1000 $ do
                _ <- traverse (const checkStatus) [1 .. 999 :: Int]
                lastThree <- traverse (const checkStatus) [1 .. 3 :: Int]
                pure lastThree
            result `shouldBe` [Accepted, FirstOverflow, SubsequentOverflow]

        it "interleaved key access works correctly" do
            result <- runQuotaTest 2 $ do
                s1 <- checkStatusForKey @Int 1
                s2 <- checkStatusForKey @Int 2
                s3 <- checkStatusForKey @Int 1
                s4 <- checkStatusForKey @Int 2
                s5 <- checkStatusForKey @Int 1
                pure [s1, s2, s3, s4, s5]
            -- Pattern: A, B, A, B, A
            -- Expected: Accepted(1), Accepted(2), Accepted(1), Accepted(2), FirstOverflow(1)
            result `shouldBe` [Accepted, Accepted, Accepted, Accepted, FirstOverflow]

    describe "Action Execution" do
        it "continuation is called with correct count and status" do
            result <- runQuotaTest 2 $ do
                results <-
                    traverse
                        ( \_ ->
                            withQuotaCheck @Int 1 $ \count status ->
                                pure (count, status)
                        )
                        [1 .. 4 :: Int]
                pure results
            result
                `shouldBe` [ (1, Accepted)
                           , (2, Accepted)
                           , (3, FirstOverflow)
                           , (4, SubsequentOverflow)
                           ]

        it "continuation with side effects executes correctly" do
            result <- runQuotaTest 2 $ do
                ref <- liftIO $ IORef.newIORef ([] :: [(Int, MessageStatus)])
                _ <-
                    traverse
                        ( \_ ->
                            withQuotaCheck @Int 1 $ \count status -> do
                                liftIO $ IORef.modifyIORef' ref ((count, status) :)
                                pure ()
                        )
                        [1 .. 3 :: Int]
                liftIO $ reverse <$> IORef.readIORef ref
            result `shouldBe` [(1, Accepted), (2, Accepted), (3, FirstOverflow)]


--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Run a quota test with the given max messages
runQuotaTest :: Int -> Eff '[Quota Int, Reader Config, Clock, Conc, Tracing, Concurrent, IOE] a -> IO a
runQuotaTest maxMessages action =
    runEff
        . runConcurrent
        . runTracingNoOp
        . runConc
        . runClock
        . runReader (Config {maxBlocksPerPeerPerSlot = maxMessages, entryTtl = 3600, cleanupInterval = 3600})
        . runQuota @Int
        $ action


-- | Check the quota for the default test key (1) and return the count
checkKey :: (Quota Int :> es) => Eff es Int
checkKey = withQuotaCheck @Int 1 $ \count _ -> pure count


-- | Check the quota for the default test key (1) and return the status
checkStatus :: (Quota Int :> es) => Eff es MessageStatus
checkStatus = withQuotaCheck @Int 1 $ \_ status -> pure status


-- | Check the quota for a specific key and return the status
checkStatusForKey :: (Hashable key, Ord key, Quota key :> es, Show key) => key -> Eff es MessageStatus
checkStatusForKey key = withQuotaCheck key $ \_ status -> pure status
