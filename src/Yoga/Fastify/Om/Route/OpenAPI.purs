module Yoga.Fastify.Om.Route.OpenAPI
  ( class RenderHeadersSchema
  , renderHeadersSchema
  , class RenderHeadersSchemaRL
  , renderHeadersSchemaRL
  , class RenderPathParamsSchema
  , renderPathParamsSchema
  , class RenderPathParamsSchemaRL
  , renderPathParamsSchemaRL
  , class RenderQueryParamsSchema
  , renderQueryParamsSchema
  , class RenderQueryParamsSchemaRL
  , renderQueryParamsSchemaRL
  , class RenderRequestBodySchema
  , renderRequestBodySchema
  , class RenderResponseHeadersSchema
  , renderResponseHeadersSchema
  , class RenderResponseHeadersSchemaRL
  , renderResponseHeadersSchemaRL
  , class RenderResponseSchema
  , renderResponseSchema
  , class RenderVariantResponseSchema
  , renderVariantResponseSchema
  , class RenderVariantResponseSchemaRL
  , renderVariantResponseSchemaRL
  , class RenderJSONSchema
  , renderJSONSchema
  , class RenderRecordSchemaRL
  , renderRecordSchemaRL
  , getRequiredFields
  , class DetectSecurity
  , detectSecurity
  , class DetectSecurityRL
  , detectSecurityRL
  , class ToOpenAPI
  , toOpenAPIImpl
  , toOpenAPI
  , class CollectOperations
  , collectOperations
  , OperationEntry
  , buildOpenAPISpec
  , buildOpenAPISpec'
  , ServerObject
  , OpenAPISpec
  , ResponseHeaderObject
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Foreign (Foreign)
import Foreign.Object as FObject
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Fastify.Om.Route.BearerToken (BearerToken)
import Yoga.Fastify.Om.Route.Encoding (JSON, FormData, NoBody)
import Yoga.Fastify.Om.Route.HeaderValue (class HeaderValueType, headerValueType)
import Yoga.Fastify.Om.Route.OpenAPIMetadata (Description, Example, Format, Minimum, Maximum, Pattern, MinLength, MaxLength, Title, Nullable, Default, Deprecated, Enum, class HasDescription, description, class HasExample, example, class HasFormat, format, class HasDeprecated, deprecated, class HasMinimum, minimum, class HasMaximum, maximum, class HasPattern, pattern, class HasMinLength, minLength, class HasMaxLength, maxLength, class HasTitle, title, class HasNullable, nullable, class HasDefault, default, class HasEnum, enum, class HasOperationMetadata, operationMetadata)
import Yoga.Fastify.Om.Route.Response (class ToResponse)
import Yoga.Fastify.Om.Route.StatusCode (class StatusCodeMap, statusCodeFor, statusCodeToString)
import Yoga.JSON (class WriteForeign, writeImpl)

--------------------------------------------------------------------------------
-- Helper Functions for Building OpenAPI Objects
--------------------------------------------------------------------------------

-- | Build a schema object with all supported OpenAPI schema properties
buildSchema
  :: { type :: String
     , format :: Maybe String
     , example :: Maybe String
     , minimum :: Maybe Int
     , maximum :: Maybe Int
     , pattern :: Maybe String
     , minLength :: Maybe Int
     , maxLength :: Maybe Int
     , title :: Maybe String
     , nullable :: Boolean
     , default :: Maybe String
     , enum :: Maybe (Array String)
     }
  -> Foreign
buildSchema s =
  let
    ins :: forall a. String -> a -> FObject.Object Foreign -> FObject.Object Foreign
    ins key val obj = FObject.insert key (unsafeCoerce val) obj

    insMaybe :: forall a. String -> Maybe a -> FObject.Object Foreign -> FObject.Object Foreign
    insMaybe key valMaybe obj = maybe obj (\v -> ins key v obj) valMaybe
    base = FObject.fromFoldable [ Tuple "type" (unsafeCoerce s.type) ]
  in
    unsafeCoerce $ base
      # insMaybe "format" s.format
      # insMaybe "example" s.example
      # insMaybe "minimum" s.minimum
      # insMaybe "maximum" s.maximum
      # insMaybe "pattern" s.pattern
      # insMaybe "minLength" s.minLength
      # insMaybe "maxLength" s.maxLength
      # insMaybe "title" s.title
      # (if s.nullable then ins "nullable" true else identity)
      # insMaybe "default" s.default
      # insMaybe "enum" s.enum

-- | Build a parameter object with all metadata
buildParameter
  :: { name :: String
     , in :: String
     , required :: Boolean
     , schema :: Foreign
     , description :: Maybe String
     , deprecated :: Maybe Boolean
     }
  -> Foreign
buildParameter { name, in: inStr, required, schema, description: descMaybe, deprecated: deprecatedMaybe } =
  let
    base = FObject.fromFoldable
      [ Tuple "name" (unsafeCoerce name)
      , Tuple "in" (unsafeCoerce inStr)
      , Tuple "required" (unsafeCoerce required)
      , Tuple "schema" schema
      ]
    withDesc = maybe base (\d -> FObject.insert "description" (unsafeCoerce d) base) descMaybe
    withDeprecated = maybe withDesc (\dep -> FObject.insert "deprecated" (unsafeCoerce dep) withDesc) deprecatedMaybe
  in
    unsafeCoerce withDeprecated

--------------------------------------------------------------------------------
-- OpenAPI Generation
--------------------------------------------------------------------------------

-- | Render headers row as OpenAPI parameter array
class RenderHeadersSchema (headers :: Row Type) where
  renderHeadersSchema :: Proxy headers -> Array Foreign

instance (RowToList headers rl, RenderHeadersSchemaRL rl headers) => RenderHeadersSchema headers where
  renderHeadersSchema _ = renderHeadersSchemaRL (Proxy :: Proxy rl)

-- | Helper class using RowList
class RenderHeadersSchemaRL (rl :: RowList Type) (headers :: Row Type) | rl -> headers where
  renderHeadersSchemaRL :: Proxy rl -> Array Foreign

instance RenderHeadersSchemaRL RL.Nil () where
  renderHeadersSchemaRL _ = []

-- Required header (non-Maybe type)
instance
  ( IsSymbol name
  , HeaderValueType ty
  , HasDescription ty
  , HasExample ty
  , HasFormat ty
  , HasMinimum ty
  , HasMaximum ty
  , HasPattern ty
  , HasMinLength ty
  , HasMaxLength ty
  , HasTitle ty
  , HasNullable ty
  , HasDefault ty
  , HasDeprecated ty
  , HasEnum ty
  , RenderHeadersSchemaRL tail tailRow
  , Row.Cons name ty tailRow headers
  , Row.Lacks name tailRow
  ) =>
  RenderHeadersSchemaRL (RL.Cons name ty tail) headers where
  renderHeadersSchemaRL _ =
    let
      p = Proxy :: Proxy ty
      schema = buildSchema
        { type: headerValueType p
        , format: format p
        , example: example p
        , minimum: minimum p
        , maximum: maximum p
        , pattern: pattern p
        , minLength: minLength p
        , maxLength: maxLength p
        , title: title p
        , nullable: nullable p
        , default: default p
        , enum: enum p
        }
      param = buildParameter
        { name: reflectSymbol (Proxy :: Proxy name)
        , in: "header"
        , required: true
        , schema
        , description: description p
        , deprecated: if deprecated p then Just true else Nothing
        }
      rest = renderHeadersSchemaRL (Proxy :: Proxy tail)
    in
      [ param ] <> rest

--------------------------------------------------------------------------------
-- Security Detection
--------------------------------------------------------------------------------

-- | Detect security requirements in request headers
-- | Returns an array of security requirement objects (e.g., [{ bearerAuth: [] }])
class DetectSecurity (headers :: Row Type) where
  detectSecurity :: Proxy headers -> Array Foreign

instance (RowToList headers rl, DetectSecurityRL rl) => DetectSecurity headers where
  detectSecurity _ = detectSecurityRL (Proxy :: Proxy rl)

-- | Helper class using RowList to find security-related headers
class DetectSecurityRL (rl :: RowList Type) where
  detectSecurityRL :: Proxy rl -> Array Foreign

-- Base case: no headers = no security
instance DetectSecurityRL RL.Nil where
  detectSecurityRL _ = []

-- Case: BearerToken found (with any metadata wrappers)
instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name BearerToken tail) where
  detectSecurityRL _ =
    let
      bearerAuth = unsafeCoerce $ FObject.singleton "bearerAuth" ([] :: Array String)
    in
      [ bearerAuth ]

-- Case: BearerToken wrapped in Description
else instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name (Description desc BearerToken) tail) where
  detectSecurityRL _ =
    let
      bearerAuth = unsafeCoerce $ FObject.singleton "bearerAuth" ([] :: Array String)
    in
      [ bearerAuth ]

-- Case: BearerToken wrapped in Example
else instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name (Example ex BearerToken) tail) where
  detectSecurityRL _ =
    let
      bearerAuth = unsafeCoerce $ FObject.singleton "bearerAuth" ([] :: Array String)
    in
      [ bearerAuth ]

-- Case: BearerToken wrapped in Deprecated
else instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name (Deprecated BearerToken) tail) where
  detectSecurityRL _ =
    let
      bearerAuth = unsafeCoerce $ FObject.singleton "bearerAuth" ([] :: Array String)
    in
      [ bearerAuth ]

-- Case: Non-security header, recurse
else instance (DetectSecurityRL tail, HeaderValueType ty) => DetectSecurityRL (RL.Cons name ty tail) where
  detectSecurityRL _ = detectSecurityRL (Proxy :: Proxy tail)

--------------------------------------------------------------------------------
-- Path Parameters Schema Generation
--------------------------------------------------------------------------------

-- | Render path parameters row as OpenAPI parameter array
class RenderPathParamsSchema (params :: Row Type) where
  renderPathParamsSchema :: Proxy params -> Array Foreign

instance (RowToList params rl, RenderPathParamsSchemaRL rl params) => RenderPathParamsSchema params where
  renderPathParamsSchema _ = renderPathParamsSchemaRL (Proxy :: Proxy rl)

-- | Helper class using RowList
class RenderPathParamsSchemaRL (rl :: RowList Type) (params :: Row Type) | rl -> params where
  renderPathParamsSchemaRL :: Proxy rl -> Array Foreign

instance RenderPathParamsSchemaRL RL.Nil () where
  renderPathParamsSchemaRL _ = []

-- Path parameters are always required
instance
  ( IsSymbol name
  , HeaderValueType ty
  , HasDescription ty
  , HasExample ty
  , HasFormat ty
  , HasMinimum ty
  , HasMaximum ty
  , HasPattern ty
  , HasMinLength ty
  , HasMaxLength ty
  , HasTitle ty
  , HasNullable ty
  , HasDefault ty
  , HasDeprecated ty
  , HasEnum ty
  , RenderPathParamsSchemaRL tail tailRow
  , Row.Cons name ty tailRow params
  , Row.Lacks name tailRow
  ) =>
  RenderPathParamsSchemaRL (RL.Cons name ty tail) params where
  renderPathParamsSchemaRL _ =
    let
      p = Proxy :: Proxy ty
      schema = buildSchema
        { type: headerValueType p
        , format: format p
        , example: example p
        , minimum: minimum p
        , maximum: maximum p
        , pattern: pattern p
        , minLength: minLength p
        , maxLength: maxLength p
        , title: title p
        , nullable: nullable p
        , default: default p
        , enum: enum p
        }
      param = buildParameter
        { name: reflectSymbol (Proxy :: Proxy name)
        , in: "path"
        , required: true
        , schema
        , description: description p
        , deprecated: if deprecated p then Just true else Nothing
        }
      rest = renderPathParamsSchemaRL (Proxy :: Proxy tail)
    in
      [ param ] <> rest

--------------------------------------------------------------------------------
-- Query Parameters Schema Generation
--------------------------------------------------------------------------------

-- | Render query parameters row as OpenAPI parameter array
class RenderQueryParamsSchema (params :: Row Type) where
  renderQueryParamsSchema :: Proxy params -> Array Foreign

instance (RowToList params rl, RenderQueryParamsSchemaRL rl params) => RenderQueryParamsSchema params where
  renderQueryParamsSchema _ = renderQueryParamsSchemaRL (Proxy :: Proxy rl)

-- | Helper class using RowList
class RenderQueryParamsSchemaRL (rl :: RowList Type) (params :: Row Type) | rl -> params where
  renderQueryParamsSchemaRL :: Proxy rl -> Array Foreign

instance RenderQueryParamsSchemaRL RL.Nil () where
  renderQueryParamsSchemaRL _ = []

-- Query parameter instance
instance
  ( IsSymbol name
  , HeaderValueType ty
  , HasDescription ty
  , HasExample ty
  , HasFormat ty
  , HasMinimum ty
  , HasMaximum ty
  , HasPattern ty
  , HasMinLength ty
  , HasMaxLength ty
  , HasTitle ty
  , HasNullable ty
  , HasDefault ty
  , HasDeprecated ty
  , HasEnum ty
  , RenderQueryParamsSchemaRL tail tailRow
  , Row.Cons name ty tailRow params
  , Row.Lacks name tailRow
  ) =>
  RenderQueryParamsSchemaRL (RL.Cons name ty tail) params where
  renderQueryParamsSchemaRL _ =
    let
      p = Proxy :: Proxy ty
      schema = buildSchema
        { type: headerValueType p
        , format: format p
        , example: example p
        , minimum: minimum p
        , maximum: maximum p
        , pattern: pattern p
        , minLength: minLength p
        , maxLength: maxLength p
        , title: title p
        , nullable: nullable p
        , default: default p
        , enum: enum p
        }
      param = buildParameter
        { name: reflectSymbol (Proxy :: Proxy name)
        , in: "query"
        , required: false
        , schema
        , description: description p
        , deprecated: if deprecated p then Just true else Nothing
        }
      rest = renderQueryParamsSchemaRL (Proxy :: Proxy tail)
    in
      [ param ] <> rest

--------------------------------------------------------------------------------
-- JSON Schema Generation (Type Introspection)
--------------------------------------------------------------------------------

-- | Render a PureScript type as an OpenAPI JSON schema
class RenderJSONSchema ty where
  renderJSONSchema :: Proxy ty -> Foreign

-- Primitive types
instance RenderJSONSchema String where
  renderJSONSchema _ = buildSchema
    { type: "string"
    , format: Nothing
    , example: Nothing
    , minimum: Nothing
    , maximum: Nothing
    , pattern: Nothing
    , minLength: Nothing
    , maxLength: Nothing
    , title: Nothing
    , nullable: false
    , default: Nothing
    , enum: Nothing
    }

instance RenderJSONSchema Int where
  renderJSONSchema _ = buildSchema
    { type: "integer"
    , format: Nothing
    , example: Nothing
    , minimum: Nothing
    , maximum: Nothing
    , pattern: Nothing
    , minLength: Nothing
    , maxLength: Nothing
    , title: Nothing
    , nullable: false
    , default: Nothing
    , enum: Nothing
    }

instance RenderJSONSchema Number where
  renderJSONSchema _ = buildSchema
    { type: "number"
    , format: Nothing
    , example: Nothing
    , minimum: Nothing
    , maximum: Nothing
    , pattern: Nothing
    , minLength: Nothing
    , maxLength: Nothing
    , title: Nothing
    , nullable: false
    , default: Nothing
    , enum: Nothing
    }

instance RenderJSONSchema Boolean where
  renderJSONSchema _ = buildSchema
    { type: "boolean"
    , format: Nothing
    , example: Nothing
    , minimum: Nothing
    , maximum: Nothing
    , pattern: Nothing
    , minLength: Nothing
    , maxLength: Nothing
    , title: Nothing
    , nullable: false
    , default: Nothing
    , enum: Nothing
    }

instance renderJSONSchemaUnit :: RenderJSONSchema Unit where
  renderJSONSchema _ = buildSchema
    { type: "null"
    , format: Nothing
    , example: Nothing
    , minimum: Nothing
    , maximum: Nothing
    , pattern: Nothing
    , minLength: Nothing
    , maxLength: Nothing
    , title: Nothing
    , nullable: false
    , default: Nothing
    , enum: Nothing
    }

-- Array type
instance renderJSONSchemaArray :: RenderJSONSchema a => RenderJSONSchema (Array a) where
  renderJSONSchema _ =
    let
      itemSchema = renderJSONSchema (Proxy :: Proxy a)
      base = FObject.fromFoldable
        [ Tuple "type" (unsafeCoerce "array")
        , Tuple "items" itemSchema
        ]
    in
      unsafeCoerce base

-- Maybe type (nullable)
instance renderJSONSchemaMaybe :: RenderJSONSchema a => RenderJSONSchema (Maybe a) where
  renderJSONSchema _ = renderJSONSchema (Proxy :: Proxy a)
    # \schema ->
        let
          obj = unsafeCoerce schema :: FObject.Object Foreign
        in
          unsafeCoerce $ FObject.insert "nullable" (unsafeCoerce true) obj

-- Record type
instance renderJSONSchemaRecord :: (RowToList row rl, RenderRecordSchemaRL rl row) => RenderJSONSchema (Record row) where
  renderJSONSchema _ =
    let
      properties = renderRecordSchemaRL (Proxy :: Proxy rl)
      required = getRequiredFields (Proxy :: Proxy rl)
      base = FObject.fromFoldable
        [ Tuple "type" (unsafeCoerce "object")
        , Tuple "properties" (unsafeCoerce properties)
        ]
      withRequired =
        if required == [] then base
        else FObject.insert "required" (unsafeCoerce required) base
    in
      unsafeCoerce withRequired

-- JSON encoding wrapper (unwrap and render inner type)
instance RenderJSONSchema a => RenderJSONSchema (JSON a) where
  renderJSONSchema _ = renderJSONSchema (Proxy :: Proxy a)

-- FormData encoding wrapper (unwrap and render inner type)
instance RenderJSONSchema a => RenderJSONSchema (FormData a) where
  renderJSONSchema _ = renderJSONSchema (Proxy :: Proxy a)

-- NoBody encoding (no schema)
instance RenderJSONSchema NoBody where
  renderJSONSchema _ = unsafeCoerce { type: "null" }

-- Metadata wrapper instances: Each unwraps and enriches the inner type's schema with its metadata
instance (RenderJSONSchema a, IsSymbol desc) => RenderJSONSchema (Description desc a) where
  renderJSONSchema _ =
    let
      innerSchema = renderJSONSchema (Proxy :: Proxy a)
      innerObj = unsafeCoerce innerSchema :: FObject.Object Foreign
      descStr = reflectSymbol (Proxy :: Proxy desc)
      withDesc =
        if descStr == "" then innerObj
        else FObject.insert "description" (unsafeCoerce descStr) innerObj
    in
      unsafeCoerce withDesc

instance (RenderJSONSchema a, IsSymbol ex) => RenderJSONSchema (Example ex a) where
  renderJSONSchema _ =
    let
      innerSchema = renderJSONSchema (Proxy :: Proxy a)
      innerObj = unsafeCoerce innerSchema :: FObject.Object Foreign
      exStr = reflectSymbol (Proxy :: Proxy ex)
      withEx =
        if exStr == "" then innerObj
        else FObject.insert "example" (unsafeCoerce exStr) innerObj
    in
      unsafeCoerce withEx

instance (RenderJSONSchema a, IsSymbol fmt) => RenderJSONSchema (Format fmt a) where
  renderJSONSchema _ =
    let
      innerSchema = renderJSONSchema (Proxy :: Proxy a)
      innerObj = unsafeCoerce innerSchema :: FObject.Object Foreign
      fmtStr = reflectSymbol (Proxy :: Proxy fmt)
      withFmt =
        if fmtStr == "" then innerObj
        else FObject.insert "format" (unsafeCoerce fmtStr) innerObj
    in
      unsafeCoerce withFmt

instance RenderJSONSchema a => RenderJSONSchema (Nullable a) where
  renderJSONSchema _ =
    let
      innerSchema = renderJSONSchema (Proxy :: Proxy a)
      innerObj = unsafeCoerce innerSchema :: FObject.Object Foreign
      withNullable = FObject.insert "nullable" (unsafeCoerce true) innerObj
    in
      unsafeCoerce withNullable

instance RenderJSONSchema a => RenderJSONSchema (Deprecated a) where
  renderJSONSchema _ =
    let
      innerSchema = renderJSONSchema (Proxy :: Proxy a)
      innerObj = unsafeCoerce innerSchema :: FObject.Object Foreign
      withDeprecated = FObject.insert "deprecated" (unsafeCoerce true) innerObj
    in
      unsafeCoerce withDeprecated

instance (RenderJSONSchema a, HasEnum (Enum a)) => RenderJSONSchema (Enum a) where
  renderJSONSchema _ =
    let
      innerSchema = renderJSONSchema (Proxy :: Proxy a)
      innerObj = unsafeCoerce innerSchema :: FObject.Object Foreign
      enumValues = enum (Proxy :: Proxy (Enum a))
      withEnum = case enumValues of
        Nothing -> innerObj
        Just vals -> FObject.insert "enum" (unsafeCoerce vals) innerObj
    in
      unsafeCoerce withEnum

-- Other metadata wrappers (Minimum, Maximum, Pattern, MinLength, MaxLength, Title, Default)
-- are similar but require more complex constraints, so we'll skip them for now in body schemas

-- | Helper class to render record fields as OpenAPI properties
class RenderRecordSchemaRL (rl :: RowList Type) (row :: Row Type) | rl -> row where
  renderRecordSchemaRL :: Proxy rl -> FObject.Object Foreign
  getRequiredFields :: Proxy rl -> Array String

instance RenderRecordSchemaRL RL.Nil () where
  renderRecordSchemaRL _ = FObject.empty
  getRequiredFields _ = []

-- Required field (non-Maybe)
instance
  ( IsSymbol name
  , RenderJSONSchema ty
  , RenderRecordSchemaRL tail tailRow
  , Row.Cons name ty tailRow row
  , Row.Lacks name tailRow
  ) =>
  RenderRecordSchemaRL (RL.Cons name ty tail) row where
  renderRecordSchemaRL _ =
    let
      fieldName = reflectSymbol (Proxy :: Proxy name)
      fieldSchema = renderJSONSchema (Proxy :: Proxy ty)
      rest = renderRecordSchemaRL (Proxy :: Proxy tail)
    in
      FObject.insert fieldName fieldSchema rest
  getRequiredFields _ =
    let
      fieldName = reflectSymbol (Proxy :: Proxy name)
      rest = getRequiredFields (Proxy :: Proxy tail)
    in
      [ fieldName ] <> rest

--------------------------------------------------------------------------------
-- Request Body Schema Generation
--------------------------------------------------------------------------------

-- | Render request body schema for OpenAPI requestBody section
-- | Returns Nothing for NoBody, Just requestBody object for JSON/FormData
class RenderRequestBodySchema (encoding :: Type) where
  renderRequestBodySchema :: Proxy encoding -> Maybe { required :: Boolean, content :: Foreign }

-- NoBody: no request body
instance RenderRequestBodySchema NoBody where
  renderRequestBodySchema _ = Nothing

-- JSON: request body with application/json content type
instance RenderJSONSchema a => RenderRequestBodySchema (JSON a) where
  renderRequestBodySchema _ = Just
    { required: true
    , content: unsafeCoerce
        { "application/json":
            { schema: renderJSONSchema (Proxy :: Proxy a) }
        }
    }

-- FormData: request body with application/x-www-form-urlencoded content type
instance RenderJSONSchema a => RenderRequestBodySchema (FormData a) where
  renderRequestBodySchema _ = Just
    { required: true
    , content: unsafeCoerce
        { "application/x-www-form-urlencoded":
            { schema: renderJSONSchema (Proxy :: Proxy a) }
        }
    }

--------------------------------------------------------------------------------
-- Response Headers Schema Generation
--------------------------------------------------------------------------------

-- | Response header object type with all metadata
type ResponseHeaderObject =
  { schema :: Foreign
  , description :: Maybe String
  , example :: Maybe String
  , deprecated :: Maybe Boolean
  }

-- | Build a response header object with metadata
buildResponseHeader
  :: { type :: String
     , description :: Maybe String
     , example :: Maybe String
     , deprecated :: Maybe Boolean
     , format :: Maybe String
     , enum :: Maybe (Array String)
     }
  -> ResponseHeaderObject
buildResponseHeader { type: typeStr, description: desc, example: ex, deprecated: dep, format: fmt, enum: enumVals } =
  let
    ins :: forall a. String -> a -> FObject.Object Foreign -> FObject.Object Foreign
    ins key val obj = FObject.insert key (unsafeCoerce val) obj

    insMaybe :: forall a. String -> Maybe a -> FObject.Object Foreign -> FObject.Object Foreign
    insMaybe key valMaybe obj = maybe obj (\v -> ins key v obj) valMaybe

    schema = unsafeCoerce $ FObject.fromFoldable [ Tuple "type" (unsafeCoerce typeStr) ]
      # insMaybe "format" fmt
      # insMaybe "enum" enumVals
  in
    { schema
    , description: desc
    , example: ex
    , deprecated: if dep == Just true then Just true else Nothing
    }

-- | Render response headers row as OpenAPI header object (for responses section)
class RenderResponseHeadersSchema (headers :: Row Type) where
  renderResponseHeadersSchema :: Proxy headers -> FObject.Object ResponseHeaderObject

instance (RowToList headers rl, RenderResponseHeadersSchemaRL rl headers) => RenderResponseHeadersSchema headers where
  renderResponseHeadersSchema _ = renderResponseHeadersSchemaRL (Proxy :: Proxy rl)

-- | Helper class using RowList
class RenderResponseHeadersSchemaRL (rl :: RowList Type) (headers :: Row Type) | rl -> headers where
  renderResponseHeadersSchemaRL :: Proxy rl -> FObject.Object ResponseHeaderObject

instance RenderResponseHeadersSchemaRL RL.Nil () where
  renderResponseHeadersSchemaRL _ = FObject.empty

-- Response header instance
instance
  ( IsSymbol name
  , HeaderValueType ty
  , HasDescription ty
  , HasExample ty
  , HasDeprecated ty
  , HasFormat ty
  , HasEnum ty
  , RenderResponseHeadersSchemaRL tail tailRow
  , Row.Cons name ty tailRow headers
  , Row.Lacks name tailRow
  ) =>
  RenderResponseHeadersSchemaRL (RL.Cons name ty tail) headers where
  renderResponseHeadersSchemaRL _ =
    let
      p = Proxy :: Proxy ty
      headerName = reflectSymbol (Proxy :: Proxy name)
      header = buildResponseHeader
        { type: headerValueType p
        , description: description p
        , example: example p
        , deprecated: if deprecated p then Just true else Nothing
        , format: format p
        , enum: enum p
        }
      rest = renderResponseHeadersSchemaRL (Proxy :: Proxy tail)
    in
      FObject.insert headerName header rest

--------------------------------------------------------------------------------
-- Complete Response Schema Generation
--------------------------------------------------------------------------------

-- | Render complete response object for OpenAPI (status 200 with headers and body)
class RenderResponseSchema (headers :: Row Type) (body :: Type) where
  renderResponseSchema
    :: Proxy headers
    -> Proxy body
    -> { "200" ::
           { description :: String
           , headers :: FObject.Object ResponseHeaderObject
           , content ::
               { "application/json" ::
                   { schema :: Foreign }
               }
           }
       }

instance (RenderResponseHeadersSchema headers, RenderJSONSchema body) => RenderResponseSchema headers body where
  renderResponseSchema headersProxy bodyProxy =
    let
      headers = renderResponseHeadersSchema headersProxy
      bodySchema = renderJSONSchema bodyProxy
    in
      { "200":
          { description: "Successful response"
          , headers: headers
          , content:
              { "application/json":
                  { schema: bodySchema }
              }
          }
      }

--------------------------------------------------------------------------------
-- Variant Response Schema Generation
--------------------------------------------------------------------------------

-- | Type alias for OpenAPI response object
type ResponseObject =
  { description :: String
  , headers :: FObject.Object ResponseHeaderObject
  , content :: { "application/json" :: { schema :: Foreign } }
  }

-- | Render variant response schema as OpenAPI responses object
-- | Maps each variant case to an HTTP status code and response object
class RenderVariantResponseSchema (variantRow :: Row Type) where
  renderVariantResponseSchema
    :: Proxy variantRow
    -> FObject.Object ResponseObject

instance (RowToList variantRow rl, RenderVariantResponseSchemaRL rl) => RenderVariantResponseSchema variantRow where
  renderVariantResponseSchema _ = renderVariantResponseSchemaRL (Proxy :: Proxy rl)

-- | Helper class that processes RowList for variant responses
class RenderVariantResponseSchemaRL (rl :: RowList Type) where
  renderVariantResponseSchemaRL
    :: Proxy rl
    -> FObject.Object ResponseObject

-- Base case: empty RowList
instance renderVariantResponseSchemaRLNil :: RenderVariantResponseSchemaRL Nil where
  renderVariantResponseSchemaRL _ = FObject.empty

-- Recursive case: process Response/ResponseData (or record syntax via ToResponse)
instance renderVariantResponseSchemaRLCons ::
  ( IsSymbol label
  , StatusCodeMap label
  , ToResponse recordType headers body
  , RenderResponseHeadersSchema headers
  , RenderJSONSchema body
  , RenderVariantResponseSchemaRL tail
  ) =>
  RenderVariantResponseSchemaRL (Cons label recordType tail) where
  renderVariantResponseSchemaRL _ =
    let
      statusCode = statusCodeFor (Proxy :: Proxy label)
      statusCodeStr = statusCodeToString statusCode
      headersObj = renderResponseHeadersSchema (Proxy :: Proxy headers)
      bodySchema = renderJSONSchema (Proxy :: Proxy body)
      responseObj =
        { description: "Successful response"
        , headers: headersObj
        , content:
            { "application/json":
                { schema: bodySchema }
            }
        }
      rest = renderVariantResponseSchemaRL (Proxy :: Proxy tail)
    in
      FObject.insert statusCodeStr responseObj rest

--------------------------------------------------------------------------------
-- ToOpenAPI
--------------------------------------------------------------------------------

-- | Generate complete OpenAPI operation object for a route
-- | Note: The instance for this typeclass is defined in Route.purs
-- | since it depends on the Route type defined there
class ToOpenAPI (route :: Type) where
  toOpenAPIImpl :: Proxy route -> String

toOpenAPI :: forall @a. ToOpenAPI a => String
toOpenAPI = toOpenAPIImpl (Proxy :: Proxy a)

--------------------------------------------------------------------------------
-- CollectOperations: Walk a type-level structure of routes
--------------------------------------------------------------------------------

type OperationEntry =
  { method :: String
  , path :: String
  , operation :: String
  }

-- | Collect OpenAPI operations from a type-level structure of routes.
-- | Use Tuple (/\) to combine multiple routes:
-- |   CollectOperations (RouteA /\ RouteB /\ RouteC)
class CollectOperations (routes :: Type) where
  collectOperations :: Proxy routes -> Array OperationEntry

-- Tuple: recurse into both sides
instance (CollectOperations a, CollectOperations b) => CollectOperations (Tuple a b) where
  collectOperations _ = collectOperations (Proxy :: Proxy a) <> collectOperations (Proxy :: Proxy b)

-- Note: Route instance is defined in Route.purs to avoid import cycles

--------------------------------------------------------------------------------
-- buildOpenAPISpec: Assemble a complete OpenAPI 3.0 document
--------------------------------------------------------------------------------

-- | Opaque type representing a complete OpenAPI specification document.
foreign import data OpenAPISpec :: Type

instance WriteForeign OpenAPISpec where
  writeImpl = unsafeCoerce

-- | Detect if any operations use bearer token authentication
-- | and build the securitySchemes component
buildSecuritySchemes :: Array OperationEntry -> FObject.Object Foreign
buildSecuritySchemes ops =
  let
    hasSecurity = ops # foldl checkOp false
      where
      checkOp acc entry =
        let
          opObj = unsafeCoerce (unsafeParseJSON entry.operation) :: FObject.Object Foreign
          securityField = FObject.lookup "security" opObj
        in
          acc || case securityField of
            Nothing -> false
            Just _ -> true
  in
    if hasSecurity then
      FObject.singleton "bearerAuth"
        ( unsafeCoerce
            { type: "http"
            , scheme: "bearer"
            , bearerFormat: "JWT"
            }
        )
    else
      FObject.empty

-- | Type for OpenAPI server object
type ServerObject =
  { url :: String
  , description :: Maybe String
  }

-- | Build a complete OpenAPI 3.0 spec from a type-level collection of routes.
-- |
-- | Examples:
-- |   -- Basic usage:
-- |   buildOpenAPISpec @(HealthRoute /\ UserRoute)
-- |     { title: "My API", version: "1.0.0" }
-- |
-- |   -- With servers:
-- |   buildOpenAPISpec' @(HealthRoute /\ UserRoute)
-- |     { title: "My API"
-- |     , version: "1.0.0"
-- |     , servers: Just
-- |         [ { url: "https://api.example.com", description: Just "Production server" }
-- |         , { url: "https://staging-api.example.com", description: Just "Staging server" }
-- |         ]
-- |     }
buildOpenAPISpec
  :: forall @routes
   . CollectOperations routes
  => { title :: String, version :: String }
  -> OpenAPISpec
buildOpenAPISpec info = buildOpenAPISpec' @routes info { servers: Nothing }

-- | Build a complete OpenAPI 3.0 spec with optional servers configuration.
buildOpenAPISpec'
  :: forall @routes
   . CollectOperations routes
  => { title :: String, version :: String }
  -> { servers :: Maybe (Array ServerObject) }
  -> OpenAPISpec
buildOpenAPISpec' info config =
  let
    ops = collectOperations (Proxy :: Proxy routes)
    paths = groupByPath ops
    securitySchemes = buildSecuritySchemes ops
    components =
      if FObject.isEmpty securitySchemes then Nothing
      else Just { securitySchemes }
    baseSpec = FObject.fromFoldable
      [ Tuple "openapi" (unsafeCoerce "3.0.0")
      , Tuple "info" (unsafeCoerce { title: info.title, version: info.version })
      , Tuple "paths" (unsafeCoerce paths)
      ]
    withComponents = case components of
      Nothing -> baseSpec
      Just c -> FObject.insert "components" (unsafeCoerce c) baseSpec
    withServers = case config.servers of
      Nothing -> withComponents
      Just servers -> FObject.insert "servers" (writeImpl servers) withComponents
  in
    unsafeCoerce $ withServers

-- | Convert `:param` to `{param}` for OpenAPI path format
toOpenAPIPath :: String -> String
toOpenAPIPath = replace (unsafeRegex ":([a-zA-Z][a-zA-Z0-9]*)" global) "{$1}"

-- | Group operations by path, then by method
-- | Returns FObject (FObject Foreign) — path -> method -> operation
groupByPath :: Array OperationEntry -> FObject.Object (FObject.Object Foreign)
groupByPath = foldl insertOp FObject.empty
  where
  insertOp acc entry =
    let
      pathKey = toOpenAPIPath entry.path
      methodMap = FObject.lookup pathKey acc # case _ of
        Nothing -> FObject.empty
        Just m -> m
      -- Parse the operation JSON string and strip method/path keys
      opForeign = stripKeys [ "method", "path" ] (unsafeParseJSON entry.operation)
      methodMap' = FObject.insert entry.method opForeign methodMap
    in
      FObject.insert pathKey methodMap' acc

-- FFI helpers
foreign import unsafeParseJSON :: String -> Foreign
foreign import stripKeys :: Array String -> Foreign -> Foreign
