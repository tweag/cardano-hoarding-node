module Hoard.Types.Environment
    ( Environment (..)
    , parseEnvironment
    , environmentName
    )
where

import Data.Aeson (FromJSON (..), withText)
import Data.String.Conversions (cs)

import Data.Text qualified as T


-- | Application deployment environment
data Environment
    = Dev
    | Staging
    | Prod
    | CI
    deriving stock (Bounded, Enum, Eq, Read, Show)


instance IsString Environment where
    fromString = \case
        "dev" -> Dev
        "staging" -> Staging
        "prod" -> Prod
        "ci" -> CI
        s -> error $ "Invalid environment: " <> cs s


instance FromJSON Environment where
    parseJSON = withText "Environment" $ \case
        "dev" -> pure Dev
        "staging" -> pure Staging
        "prod" -> pure Prod
        "ci" -> pure CI
        other -> fail $ "Invalid environment: " <> T.unpack other


-- | Parse environment from a string (case-insensitive)
parseEnvironment :: Text -> Maybe Environment
parseEnvironment text = case T.toLower text of
    "dev" -> Just Dev
    "staging" -> Just Staging
    "prod" -> Just Prod
    "ci" -> Just CI
    _ -> Nothing


-- | Get the lowercase name of an environment
environmentName :: Environment -> Text
environmentName = \case
    Dev -> "dev"
    Staging -> "staging"
    Prod -> "prod"
    CI -> "ci"
