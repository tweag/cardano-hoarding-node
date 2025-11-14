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
    fromString s = case parseEnvironment (cs s) of
        Just env -> env
        Nothing -> error $ "Invalid environment: " <> cs s


instance FromJSON Environment where
    parseJSON = withText "Environment" $ \x -> case parseEnvironment x of
        Just env -> pure env
        Nothing -> fail $ "Invalid environment: " <> toString x


-- | Parse environment from a string
parseEnvironment :: Text -> Maybe Environment
parseEnvironment = inverseMap environmentName . T.toLower


-- | Get the lowercase name of an environment
environmentName :: Environment -> Text
environmentName = \case
    Dev -> "dev"
    Staging -> "staging"
    Prod -> "prod"
    CI -> "ci"
