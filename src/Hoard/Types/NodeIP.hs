module Hoard.Types.NodeIP
    ( NodeIP (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)
import Data.IP (IP)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Rel8 (DBEq, DBType (..), parseTypeInformation)
import Text.Read (readMaybe)


newtype NodeIP = NodeIP {getNodeIP :: IP}
    deriving stock (Eq, Ord, Generic)
    deriving (Show, Read) via (IP)


instance FromJSON NodeIP where
    parseJSON = withText "NodeIP" $ \t ->
        case readMaybe $ T.unpack t of
            Just ip -> pure $ NodeIP ip
            Nothing -> fail "Not a valid IP string"


instance ToJSON NodeIP where
    toJSON = String . T.pack . show . getNodeIP


instance DBType NodeIP where
    typeInformation =
        parseTypeInformation
            (maybe (Left "Not a valid IP string") (Right . NodeIP) . readMaybe . T.unpack)
            (T.pack . show . getNodeIP)
            (typeInformation @Text)


instance DBEq NodeIP
