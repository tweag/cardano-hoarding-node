{-# OPTIONS_GHC -Wno-orphans #-}

module Hoard.API.OpenAPI (spec) where

import Control.Lens ((%~), (?~))
import Data.Aeson (ToJSON, toJSON)
import Data.Aeson.Types (Options, defaultOptions, fieldLabelModifier)
import Data.OpenApi
    ( Definitions
    , HasEnum (enum_)
    , HasFormat (format)
    , HasType (type_)
    , NamedSchema (..)
    , OpenApi
    , OpenApiType (..)
    , Schema
    , ToParamSchema (..)
    , ToSchema (..)
    , defaultSchemaOptions
    , fromAesonOptions
    , genericDeclareNamedSchema
    , paths
    )
import Data.OpenApi.Declare (Declare)
import Data.Typeable (tyConName, typeRep, typeRepTyCon)
import Servant.OpenApi (toOpenApi)
import Text.Casing (quietSnake)
import Web.HttpApiData (ToHttpApiData, toUrlPiece)

import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap

import Hoard.API.Data.Block (Block)
import Hoard.API.Data.BlockViolation (BlockViolation, ReceiptInfo, SlotDispute)
import Hoard.API.Data.Header (Header)
import Hoard.API.Peers (PinPeerRequest)
import Hoard.API.Routes (API)
import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.BlockTag (BlockTag)
import Hoard.Data.Eras (BlockEra)
import Hoard.Data.HeaderTag (HeaderTag)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer, PeerAddress)
import Hoard.Data.PoolID (PoolID)
import Hoard.Data.Serialized (Serialized)
import Hoard.OrphanDetection.Data (BlockClassification)
import Hoard.Types.NodeIP (NodeIP)


--------
-- Helpers
--------

quietSnakeOpts :: Options
quietSnakeOpts = defaultOptions {fieldLabelModifier = quietSnake}


-- | Schema for a named string type, with an optional format specifier.
--
-- @namedStringSchema "NodeIP" Nothing@  →  @{ "type": "string" }@
--
-- @namedStringSchema "ID" (Just "uuid")@  →  @{ "type": "string", "format": "uuid" }@
namedStringSchema :: Text -> Maybe Text -> Declare (Definitions Schema) NamedSchema
namedStringSchema name mFmt =
    pure
        $ NamedSchema (Just name)
        $ mempty
            & type_
            ?~ OpenApiString
                & maybe id (format ?~) mFmt


-- | Schema for a string enum whose values come from 'ToJSON'.
-- The schema name is derived from the type's 'Typeable' representation.
--
-- @enumSchema (Proxy \@BlockEra)@  →  @{ "type": "string", "enum": ["Byron", "Allegra", ...] }@
enumSchema
    :: forall a proxy
     . (Bounded a, Enum a, ToJSON a, Typeable a)
    => proxy a
    -> Declare (Definitions Schema) NamedSchema
enumSchema proxy =
    pure
        $ NamedSchema (Just $ toText $ tyConName $ typeRepTyCon $ typeRep proxy)
        $ mempty
            & type_
            ?~ OpenApiString
                & enum_
            ?~ map toJSON [minBound .. maxBound :: a]


-- | Param schema for a string enum whose values come from 'ToHttpApiData'.
-- Used for query parameters rather than request/response bodies.
--
-- @enumParamSchema (Proxy \@BlockTag)@  →  @{ "type": "string", "enum": ["CorruptBlockIntegrity", ...] }@
enumParamSchema
    :: forall a proxy
     . (Bounded a, Enum a, ToHttpApiData a)
    => proxy a
    -> Schema
enumParamSchema _ =
    mempty
        & type_
        ?~ OpenApiString
            & enum_
        ?~ map (toJSON . toUrlPiece) [minBound .. maxBound :: a]


--------
-- ToSchema instances
--------

-- The /openapi.json endpoint returns OpenApi itself; we need this instance to
-- satisfy HasOpenApi for the route, even though we strip the path from the
-- final spec.
instance ToSchema OpenApi where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "OpenApi") $ mempty & type_ ?~ OpenApiObject


instance ToSchema BlockHash where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)


instance ToSchema PoolID where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)


instance (Typeable (a :: Type)) => ToSchema (ID a) where
    declareNamedSchema _ = namedStringSchema "ID" (Just "uuid")


instance (Typeable (a :: Type)) => ToSchema (Serialized a) where
    declareNamedSchema _ = namedStringSchema "Serialized" (Just "byte")


instance ToSchema BlockEra where
    declareNamedSchema = enumSchema


instance ToSchema BlockClassification where
    declareNamedSchema = enumSchema


instance ToSchema NodeIP where
    declareNamedSchema _ = namedStringSchema "NodeIP" Nothing


instance ToSchema Peer where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions


instance ToSchema PeerAddress where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions


instance ToSchema Block where
    declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions quietSnakeOpts)


instance ToSchema BlockViolation where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions


instance ToSchema ReceiptInfo where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions


instance ToSchema SlotDispute where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions


instance ToSchema Header where
    declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions quietSnakeOpts)


instance ToSchema PinPeerRequest where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions


--------
-- ToParamSchema instances
--------

instance ToParamSchema BlockTag where
    toParamSchema = enumParamSchema


instance ToParamSchema HeaderTag where
    toParamSchema = enumParamSchema


--------
-- Spec
--------

excludedPaths :: [FilePath]
excludedPaths = ["/metrics", "/openapi.json"]


spec :: OpenApi
spec =
    toOpenApi (Proxy @API)
        & paths
        %~ flip (foldr InsOrdHashMap.delete) excludedPaths
