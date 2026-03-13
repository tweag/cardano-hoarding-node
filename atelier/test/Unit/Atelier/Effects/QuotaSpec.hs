module Unit.Atelier.Effects.QuotaSpec (spec_Quota) where

import Effectful (IOE, runEff)
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Concurrent.Async (mapConcurrently)
import Effectful.Reader.Static (Reader, runReader)
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.IORef qualified as IORef

import Atelier.Effects.Clock (Clock, runClock)
import Atelier.Effects.Conc (Conc, runConc)
import Atelier.Effects.Delay (Delay, runDelay)
import Atelier.Effects.Log (Log, runLogNoOp)
import Atelier.Effects.Quota (Config (..), Quota, runQuota, withQuotaCheck)


spec_Quota :: Spec
spec_Quota = do
    describe "Basic Behaviors" do
        it "count increments for each check" do
            result <- runQuotaTest $ do
                counts <- traverse (const checkCount) [1 .. 3 :: Int]
                pure counts
            result `shouldBe` [1, 2, 3]

        it "messages within quota are Accepted" do
            result <- runQuotaTest $ do
                statuses <- traverse (const $ checkStatus 3) [1 .. 3 :: Int]
                pure statuses
            result `shouldBe` [Accepted, Accepted, Accepted]

        it "first message exceeding quota is Overflow 1" do
            result <- runQuotaTest $ do
                statuses <- traverse (const $ checkStatus 2) [1 .. 3 :: Int]
                pure statuses
            result `shouldBe` [Accepted, Accepted, Overflow 1]

        it "subsequent messages increment the overflow count" do
            result <- runQuotaTest $ do
                statuses <- traverse (const $ checkStatus 2) [1 .. 5 :: Int]
                pure statuses
            result `shouldBe` [Accepted, Accepted, Overflow 1, Overflow 2, Overflow 3]

        it "count is accurate after multiple checks" do
            result <- runQuotaTest $ do
                _ <- traverse (const $ checkStatus 3) [1 .. 5 :: Int]
                finalCount <- checkCount
                pure finalCount
            result `shouldBe` 6

    describe "Multiple Keys" do
        it "different keys have independent quotas" do
            result <- runQuotaTest $ do
                key1Statuses <- traverse (const $ checkStatusForKey @Int 1 2) [1 .. 3 :: Int]
                key2Statuses <- traverse (const $ checkStatusForKey @Int 2 2) [1 .. 3 :: Int]
                pure (key1Statuses, key2Statuses)
            result
                `shouldBe` ( [Accepted, Accepted, Overflow 1]
                           , [Accepted, Accepted, Overflow 1]
                           )

        it "one key at quota doesn't affect other keys" do
            result <- runQuotaTest $ do
                _ <- traverse (const $ checkStatusForKey @Int 1 2) [1 .. 3 :: Int]
                key2Status <- checkStatusForKey @Int 2 2
                pure key2Status
            result `shouldBe` Accepted

        it "works with integer keys" do
            result <- runQuotaTest $ do
                key1Status <- checkStatusForKey @Int 1 2
                key2Status <- checkStatusForKey @Int 42 2
                key3Status <- checkStatusForKey @Int 999 2
                pure (key1Status, key2Status, key3Status)
            result `shouldBe` (Accepted, Accepted, Accepted)

    describe "Concurrent Access" do
        it "concurrent checks on same key count all messages" do
            result <- runQuotaTest $ do
                -- Run 50 concurrent checks
                _ <- mapConcurrently (const checkCount) [1 .. 50 :: Int]
                finalCount <- checkCount
                pure finalCount
            result `shouldBe` 51

        it "exactly one thread gets Overflow 1" do
            result <- runQuotaTest $ do
                -- Run 10 concurrent checks (5 within quota, 5 overflow)
                statuses <- mapConcurrently (const $ checkStatus 5) [1 .. 10 :: Int]
                let firstOverflowCount = length $ filter (== Overflow 1) statuses
                pure firstOverflowCount
            result `shouldBe` 1

        it "concurrent access to different keys doesn't interfere" do
            result <- runQuotaTest $ do
                results <-
                    mapConcurrently
                        (\i -> traverse (const $ checkStatusForKey @Int i 2) [1 .. 3 :: Int])
                        [1 .. 5 :: Int]
                pure $ fmap (filter (== Overflow 1)) results
            all (\overflows -> length overflows == 1) result `shouldBe` True

    describe "Edge Cases" do
        it "quota of 0 makes every message overflow" do
            result <- runQuotaTest $ do
                statuses <- traverse (const $ checkStatus 0) [1 .. 3 :: Int]
                pure statuses
            result `shouldBe` [Overflow 1, Overflow 2, Overflow 3]

        it "quota of 1 allows one message then overflows" do
            result <- runQuotaTest $ do
                statuses <- traverse (const $ checkStatus 1) [1 .. 3 :: Int]
                pure statuses
            result `shouldBe` [Accepted, Overflow 1, Overflow 2]

        it "large quota values work correctly" do
            result <- runQuotaTest $ do
                _ <- traverse (const $ checkStatus 1000) [1 .. 999 :: Int]
                lastThree <- traverse (const $ checkStatus 1000) [1 .. 3 :: Int]
                pure lastThree
            result `shouldBe` [Accepted, Overflow 1, Overflow 2]

        it "interleaved key access works correctly" do
            result <- runQuotaTest $ do
                s1 <- checkStatusForKey @Int 1 2
                s2 <- checkStatusForKey @Int 2 2
                s3 <- checkStatusForKey @Int 1 2
                s4 <- checkStatusForKey @Int 2 2
                s5 <- checkStatusForKey @Int 1 2
                pure [s1, s2, s3, s4, s5]
            -- Pattern: A, B, A, B, A
            -- Expected: Accepted(1), Accepted(2), Accepted(1), Accepted(2), Overflow 1(key 1)
            result `shouldBe` [Accepted, Accepted, Accepted, Accepted, Overflow 1]

    describe "Custom Classifiers" do
        it "identity classifier returns the raw hit count" do
            result <- runQuotaTest $ do
                traverse (const $ withQuotaCheck @Int 1 id pure) [1 .. 4 :: Int]
            result `shouldBe` [1, 2, 3, 4 :: Int]

        it "multi-mark classifier yields correct mark for each hit" do
            -- Two thresholds: warning at >2, critical at >4
            let classify c
                    | c > 4 = Just True -- critical
                    | c > 2 = Just False -- warning
                    | otherwise = Nothing -- ok
            result <- runQuotaTest $ do
                traverse (const $ withQuotaCheck @Int 1 classify pure) [1 .. 6 :: Int]
            result `shouldBe` [Nothing, Nothing, Just False, Just False, Just True, Just True]

        it "classifier producing tuples gives both count and status" do
            result <- runQuotaTest $ do
                traverse
                    (const $ withQuotaCheck @Int 1 (\c -> (c, messageStatusClassifier 2 c)) pure)
                    [1 .. 4 :: Int]
            result `shouldBe` [(1, Accepted), (2, Accepted), (3, Overflow 1), (4, Overflow 2)]

    describe "Action Execution" do
        it "continuation with side effects executes correctly" do
            result <- runQuotaTest $ do
                ref <- liftIO $ IORef.newIORef ([] :: [(Int, MessageStatus)])
                _ <-
                    traverse
                        ( \_ ->
                            withQuotaCheck @Int 1 (\c -> (c, messageStatusClassifier 2 c)) $ \(count, status) -> do
                                liftIO $ IORef.modifyIORef' ref ((count, status) :)
                                pure ()
                        )
                        [1 .. 3 :: Int]
                liftIO $ reverse <$> IORef.readIORef ref
            result `shouldBe` [(1, Accepted), (2, Accepted), (3, Overflow 1)]


--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Run a quota test
runQuotaTest :: Eff '[Quota Int, Reader Config, Clock, Delay, Conc, Log, Concurrent, IOE] a -> IO a
runQuotaTest action =
    runEff
        . runConcurrent
        . runLogNoOp
        . runConc
        . runDelay
        . runClock
        . runReader (Config {entryTtl = 3600, cleanupInterval = 3600})
        . runQuota @Int
        $ action


data MessageStatus = Accepted | Overflow Int deriving stock (Eq, Show)


messageStatusClassifier :: Int -> Int -> MessageStatus
messageStatusClassifier maxMessages c
    | c <= maxMessages = Accepted
    | otherwise = Overflow (c - maxMessages)


-- | Increment the quota for the default test key and return the raw count
checkCount :: (Quota Int :> es) => Eff es Int
checkCount = withQuotaCheck @Int 1 id pure


-- | Increment the quota for the default test key and return 'MessageStatus' against the given limit
checkStatus :: (Quota Int :> es) => Int -> Eff es MessageStatus
checkStatus maxMessages = withQuotaCheck @Int 1 (messageStatusClassifier maxMessages) pure


-- | Increment the quota for a specific key and return 'MessageStatus' against the given limit
checkStatusForKey :: (Hashable key, Ord key, Quota key :> es) => key -> Int -> Eff es MessageStatus
checkStatusForKey key maxMessages = withQuotaCheck key (messageStatusClassifier maxMessages) pure
