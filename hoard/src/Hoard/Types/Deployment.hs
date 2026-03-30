module Hoard.Types.Deployment
    ( Deployment (..)
    , parseDeployment
    , deploymentName
    )
where

import Data.Aeson (FromJSON (..), withText)
import Data.String.Conversions (cs)

import Data.Text qualified as T


-- | Application deployment environment
data Deployment
    = Dev
    | Staging
    | Prod
    | CI
    deriving stock (Bounded, Enum, Eq, Read, Show)


instance IsString Deployment where
    fromString s = case parseDeployment (cs s) of
        Just env -> env
        Nothing -> error $ "Invalid deployment: " <> cs s


instance FromJSON Deployment where
    parseJSON = withText "Deployment" $ \x -> case parseDeployment x of
        Just env -> pure env
        Nothing -> fail $ "Invalid deployment: " <> toString x


-- | Parse deployment from a string
parseDeployment :: Text -> Maybe Deployment
parseDeployment = inverseMap deploymentName . T.toLower


-- | Get the lowercase name of a deployment
deploymentName :: Deployment -> Text
deploymentName = \case
    Dev -> "dev"
    Staging -> "staging"
    Prod -> "prod"
    CI -> "ci"
