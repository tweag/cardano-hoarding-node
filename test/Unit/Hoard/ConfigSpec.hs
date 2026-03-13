module Unit.Hoard.ConfigSpec (spec_Config) where

import Control.Exception (finally)
import Data.Aeson (FromJSON, Value (..), object)
import Data.Default (Default (..))
import Effectful (runEff)
import Effectful.Reader.Static (ask, runReader)
import System.Environment (getEnvironment, setEnv, unsetEnv)
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)

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
        deepMerge (object [("a", Number 1)]) (Number 99) `shouldBe` Number 99

    it "left scalar is replaced by right object" do
        let r = object [("a", Number 1)]
        deepMerge (Number 99) r `shouldBe` r

    it "disjoint object keys are unioned" do
        let l = object [("a", Number 1)]
            r = object [("b", Number 2)]
        deepMerge l r `shouldBe` object [("a", Number 1), ("b", Number 2)]

    it "right wins on conflicting scalar values" do
        let l = object [("k", String "left")]
            r = object [("k", String "right")]
        deepMerge l r `shouldBe` object [("k", String "right")]

    it "nested objects are merged recursively" do
        let l = object [("x", object [("a", Number 1), ("b", Number 2)])]
            r = object [("x", object [("b", Number 99), ("c", Number 3)])]
        deepMerge l r
            `shouldBe` object [("x", object [("a", Number 1), ("b", Number 99), ("c", Number 3)])]

    it "merging with empty object is identity" do
        let v = object [("a", Number 1)]
        deepMerge v (object []) `shouldBe` v
        deepMerge (object []) v `shouldBe` v


--------------------------------------------------------------------------------
-- envOverrides
--------------------------------------------------------------------------------

envOverridesSpec :: Spec
envOverridesSpec = describe "envOverrides" $ around withCleanEnv do
    it "ignores env vars without the prefix" do
        setEnv "OTHER__FOO" "bar"
        result <- envOverrides "HOARD"
        result `shouldBe` object []

    it "single-segment key becomes top-level field" do
        setEnv "HOARD__HOST" "localhost"
        result <- envOverrides "HOARD"
        result `shouldBe` object [("host", String "localhost")]

    it "double-underscore separates path segments" do
        setEnv "HOARD__DATABASE__HOST" "db.example.com"
        result <- envOverrides "HOARD"
        result `shouldBe` object [("database", object [("host", String "db.example.com")])]

    it "segment keys are lowercased" do
        setEnv "HOARD__SERVER__HOST" "0.0.0.0"
        result <- envOverrides "HOARD"
        result `shouldBe` object [("server", object [("host", String "0.0.0.0")])]

    it "single underscores within a segment are preserved" do
        setEnv "HOARD__LOGGING__MINIMUM_SEVERITY" "INFO"
        result <- envOverrides "HOARD"
        result `shouldBe` object [("logging", object [("minimum_severity", String "INFO")])]

    it "deeply nested path" do
        setEnv "HOARD__DATABASE__USERS__READER__PASSWORD" "secret"
        result <- envOverrides "HOARD"
        result
            `shouldBe` object
                [
                    ( "database"
                    , object
                        [
                            ( "users"
                            , object [("reader", object [("password", String "secret")])]
                            )
                        ]
                    )
                ]

    it "numeric string is parsed as Number" do
        setEnv "HOARD__SERVER__PORT" "3000"
        result <- envOverrides "HOARD"
        result `shouldBe` object [("server", object [("port", Number 3000)])]

    it "boolean string is parsed as Bool" do
        setEnv "HOARD__TRACING__ENABLED" "true"
        result <- envOverrides "HOARD"
        result `shouldBe` object [("tracing", object [("enabled", Bool True)])]

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
            `shouldBe` object
                [("server", object [("host", String "0.0.0.0"), ("port", Number 3001)])]


--------------------------------------------------------------------------------
-- runConfig
--------------------------------------------------------------------------------

runConfigSpec :: Spec
runConfigSpec = describe "runConfig" do
    it "decodes value at the given key" do
        let root = object [("server", object [("host", String "127.0.0.1"), ("port", Number 9090)])]
        cfg <- runEff . runLoadedConfigReader root $ runConfig @"server" @TestServerConfig ask
        cfg `shouldBe` TestServerConfig {host = "127.0.0.1", port = 9090}

    it "falls back to Default when key is absent" do
        let root = object []
        cfg <- runEff . runLoadedConfigReader root $ runConfig @"server" @TestServerConfig ask
        cfg `shouldBe` def

    it "falls back to Default when root is not an Object" do
        let root = String "not an object"
        cfg <- runEff . runLoadedConfigReader root $ runConfig @"server" @TestServerConfig ask
        cfg `shouldBe` def

    it "decodes from a deeper merged object" do
        let base = object [("server", object [("host", String "base"), ("port", Number 80)])]
            merged = deepMerge base (object [("server", object [("port", Number 443)])])
        cfg <- runEff . runLoadedConfigReader merged $ runConfig @"server" @TestServerConfig ask
        cfg `shouldBe` TestServerConfig {host = "base", port = 443}


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------



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
