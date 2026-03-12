module Unit.Hoard.ConfigSpec (spec_Config) where

import Control.Exception (finally)
import Data.Aeson (FromJSON, Value (..))
import Data.Default (Default (..))
import Effectful (runEff)
import Effectful.Reader.Static (ask, runReader)
import System.Environment (getEnvironment, setEnv, unsetEnv)
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM

import Hoard.Effects.ConfigPath (deepMerge, envOverrides, runConfig)


spec_Config :: Spec
spec_Config = do
    deepMergeSpec
    envOverridesSpec
    runConfigSpec


--------------------------------------------------------------------------------
-- deepMerge
--------------------------------------------------------------------------------

deepMergeSpec :: Spec
deepMergeSpec = describe "deepMerge" do
    it "right scalar wins over left scalar" do
        deepMerge (Number 1) (Number 2) `shouldBe` Number 2

    it "right scalar wins over left object" do
        deepMerge (obj [("a", Number 1)]) (Number 99) `shouldBe` Number 99

    it "left scalar is replaced by right object" do
        let r = obj [("a", Number 1)]
        deepMerge (Number 99) r `shouldBe` r

    it "disjoint object keys are unioned" do
        let l = obj [("a", Number 1)]
            r = obj [("b", Number 2)]
        deepMerge l r `shouldBe` obj [("a", Number 1), ("b", Number 2)]

    it "right wins on conflicting scalar values" do
        let l = obj [("k", String "left")]
            r = obj [("k", String "right")]
        deepMerge l r `shouldBe` obj [("k", String "right")]

    it "nested objects are merged recursively" do
        let l = obj [("x", obj [("a", Number 1), ("b", Number 2)])]
            r = obj [("x", obj [("b", Number 99), ("c", Number 3)])]
        deepMerge l r
            `shouldBe` obj [("x", obj [("a", Number 1), ("b", Number 99), ("c", Number 3)])]

    it "merging with empty object is identity" do
        let v = obj [("a", Number 1)]
        deepMerge v (obj []) `shouldBe` v
        deepMerge (obj []) v `shouldBe` v


--------------------------------------------------------------------------------
-- envOverrides
--------------------------------------------------------------------------------

envOverridesSpec :: Spec
envOverridesSpec = describe "envOverrides" $ around withCleanEnv do
    it "ignores env vars without the prefix" do
        setEnv "OTHER__FOO" "bar"
        result <- envOverrides "HOARD"
        result `shouldBe` obj []

    it "single-segment key becomes top-level field" do
        setEnv "HOARD__HOST" "localhost"
        result <- envOverrides "HOARD"
        result `shouldBe` obj [("host", String "localhost")]

    it "double-underscore separates path segments" do
        setEnv "HOARD__DATABASE__HOST" "db.example.com"
        result <- envOverrides "HOARD"
        result `shouldBe` obj [("database", obj [("host", String "db.example.com")])]

    it "segment keys are lowercased" do
        setEnv "HOARD__SERVER__HOST" "0.0.0.0"
        result <- envOverrides "HOARD"
        result `shouldBe` obj [("server", obj [("host", String "0.0.0.0")])]

    it "single underscores within a segment are preserved" do
        setEnv "HOARD__LOGGING__MINIMUM_SEVERITY" "INFO"
        result <- envOverrides "HOARD"
        result `shouldBe` obj [("logging", obj [("minimum_severity", String "INFO")])]

    it "deeply nested path" do
        setEnv "HOARD__DATABASE__USERS__READER__PASSWORD" "secret"
        result <- envOverrides "HOARD"
        result
            `shouldBe` obj
                [
                    ( "database"
                    , obj
                        [
                            ( "users"
                            , obj [("reader", obj [("password", String "secret")])]
                            )
                        ]
                    )
                ]

    it "numeric string is parsed as Number" do
        setEnv "HOARD__SERVER__PORT" "3000"
        result <- envOverrides "HOARD"
        result `shouldBe` obj [("server", obj [("port", Number 3000)])]

    it "boolean string is parsed as Bool" do
        setEnv "HOARD__TRACING__ENABLED" "true"
        result <- envOverrides "HOARD"
        result `shouldBe` obj [("tracing", obj [("enabled", Bool True)])]

    it "non-numeric string stays as String" do
        setEnv "HOARD__LOGGING__MINIMUM_SEVERITY" "INFO"
        result <- envOverrides "HOARD"
        result `shouldSatisfy` \v ->
            lookupNested ["logging", "minimum_severity"] v == Just (String "INFO")

    it "multiple vars are merged into one object" do
        setEnv "HOARD__SERVER__HOST" "0.0.0.0"
        setEnv "HOARD__SERVER__PORT" "3001"
        result <- envOverrides "HOARD"
        result
            `shouldBe` obj
                [("server", obj [("host", String "0.0.0.0"), ("port", Number 3001)])]


--------------------------------------------------------------------------------
-- runConfig
--------------------------------------------------------------------------------

runConfigSpec :: Spec
runConfigSpec = describe "runConfig" do
    it "decodes value at the given key" do
        let root = obj [("server", obj [("host", String "127.0.0.1"), ("port", Number 9090)])]
        cfg <- runEff . runReader root $ runConfig @"server" @TestServerConfig ask
        cfg `shouldBe` TestServerConfig {host = "127.0.0.1", port = 9090}

    it "falls back to Default when key is absent" do
        let root = obj []
        cfg <- runEff . runReader root $ runConfig @"server" @TestServerConfig ask
        cfg `shouldBe` def

    it "falls back to Default when root is not an Object" do
        let root = String "not an object"
        cfg <- runEff . runReader root $ runConfig @"server" @TestServerConfig ask
        cfg `shouldBe` def

    it "decodes from a deeper merged object" do
        let base = obj [("server", obj [("host", String "base"), ("port", Number 80)])]
            merged = deepMerge base (obj [("server", obj [("port", Number 443)])])
        cfg <- runEff . runReader merged $ runConfig @"server" @TestServerConfig ask
        cfg `shouldBe` TestServerConfig {host = "base", port = 443}


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

obj :: [(Aeson.Key, Value)] -> Value
obj = Object . KM.fromList


lookupNested :: [Text] -> Value -> Maybe Value
lookupNested [] v = Just v
lookupNested (k : ks) (Object m) = KM.lookup (fromString (toString k)) m >>= lookupNested ks
lookupNested _ _ = Nothing


-- | Unset all HOARD__ vars before and after each test that touches the environment.
withCleanEnv :: (() -> IO ()) -> IO ()
withCleanEnv test = do
    cleanHoardEnv
    test () `finally` cleanHoardEnv
  where
    cleanHoardEnv = do
        env <- getEnvironment
        traverse_ (unsetEnv . fst) $ filter (("HOARD__" `isPrefixOf`) . fst) env


-- | Minimal config type for testing runConfig.
data TestServerConfig = TestServerConfig
    { host :: Text
    , port :: Int
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)


instance Default TestServerConfig where
    def = TestServerConfig {host = "0.0.0.0", port = 3000}
