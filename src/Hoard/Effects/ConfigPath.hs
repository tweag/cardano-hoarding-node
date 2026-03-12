{-# LANGUAGE AllowAmbiguousTypes #-}

module Hoard.Effects.ConfigPath
    ( envOverrides
    , deepMerge
    , loadHoardConfig
    , runConfigRoot
    , runConfig
    ) where

import Data.Aeson (FromJSON (..), Value (..))
import Data.Default (Default (..))
import Effectful (IOE)
import Effectful.Exception (throwIO)
import Effectful.Reader.Static (Reader, asks, runReader)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO.Error (userError)

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Yaml qualified as Yaml

import Hoard.Effects.Options (Options)
import Hoard.Types.Deployment (Deployment (..), deploymentName)

import Hoard.Effects.Options qualified as Options


-- | Build a nested JSON Object from environment variables with the given prefix.
-- Variables are expected in the form PREFIX__SEG1__SEG2=value.
-- Double underscore is the path separator; single underscore is preserved within
-- segments.
envOverrides :: Text -> IO Value
envOverrides prefix = do
    allEnv <- getEnvironment
    let prefixStr = toString prefix <> "__"
        matching = [(k, v) | (k, v) <- allEnv, prefixStr `isPrefixOf` k]
        pairs = [(splitPath (drop (length prefixStr) k), parseScalar (toText v)) | (k, v) <- matching]
    pure $ foldl' insertNested (Object KM.empty) pairs
  where
    splitPath :: String -> [Text]
    splitPath = map T.toLower . T.splitOn "__" . toText

    insertNested :: Value -> ([Text], Value) -> Value
    insertNested base ([], _) = base
    insertNested base ([k], v) =
        let key = fromString (toString k)
        in  Object $ case base of
                Object m -> KM.insert key v m
                _ -> KM.singleton key v
    insertNested base (k : ks, v) =
        let key = fromString (toString k)
            child = case base of
                Object m -> fromMaybe (Object KM.empty) (KM.lookup key m)
                _ -> Object KM.empty
            merged = insertNested child (ks, v)
        in  Object $ case base of
                Object m -> KM.insert key merged m
                _ -> KM.singleton key merged


-- | Parse a scalar text value as its natural JSON type when possible,
-- falling back to a JSON String.
parseScalar :: Text -> Value
parseScalar t =
    fromMaybe (String t)
        $ Aeson.decode (BL.fromStrict (TE.encodeUtf8 t))


-- | Deep merge two JSON values. Right wins on conflict for non-Object values;
-- Objects are merged recursively.
deepMerge :: Value -> Value -> Value
deepMerge (Object l) (Object r) = Object $ KM.unionWith deepMerge l r
deepMerge _ r = r


-- | Load the full configuration for a deployment.
-- Priority (highest to lowest): env vars > secrets/{env}.yaml > config/{env}.yaml.
loadHoardConfig :: Deployment -> IO Value
loadHoardConfig deployment = do
    let env = toString (deploymentName deployment)
        configYaml = "config" </> env <> ".yaml"
        secretsYaml = "secrets" </> env <> ".yaml"
    base <- loadOptional configYaml
    secrets <- loadOptional secretsYaml
    envVars <- envOverrides "HOARD"
    pure $ deepMerge (deepMerge base secrets) envVars
  where
    loadOptional path =
        doesFileExist path >>= \case
            True -> Yaml.decodeFileThrow path
            False -> pure (Object KM.empty)


runConfigRoot :: (IOE :> es, Reader Options :> es) => Eff (Reader Value : es) a -> Eff es a
runConfigRoot eff = do
    deployment <- asks $ fromMaybe Dev . Options.deployment
    root <- liftIO $ loadHoardConfig deployment
    runReader root eff


runConfig
    :: forall (key :: Symbol) r es a
     . (Default r, FromJSON r, KnownSymbol key, Reader Value :> es)
    => Eff (Reader r : es) a
    -> Eff es a
runConfig eff = do
    root <- asks @Value id
    r <- case root of
        Aeson.Object m ->
            case KM.lookup (fromString (symbolVal (Proxy @key))) m of
                Nothing -> pure def
                Just v -> case Aeson.fromJSON v of
                    Aeson.Success a -> pure a
                    Aeson.Error e ->
                        throwIO
                            $ userError
                            $ "Failed to parse config key '" <> symbolVal (Proxy @key) <> "': " <> e
        _ -> pure def
    runReader r eff
