module Yoga.HTTP.API.Route.OpenAPI
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
  , class RenderCookieParamsSchema
  , renderCookieParamsSchema
  , class RenderCookieParamsSchemaRL
  , renderCookieParamsSchemaRL
  , class GetContentType
  , getContentType
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
  , class RenderResponseContent
  , renderResponseContent
  , class RenderJSONSchema
  , renderJSONSchema
  , class RenderRecordSchemaRL
  , renderRecordSchemaRL
  , getRequiredFields
  , class DetectSecurity
  , detectSecurity
  , class DetectSecurityRL
  , detectSecurityRL
  , class DetectCookieSecurity
  , detectCookieSecurity
  , class DetectCookieSecurityRL
  , detectCookieSecurityRL
  , class ToOpenAPI
  , toOpenAPIImpl
  , toOpenAPI
  , class CollectOperations
  , collectOperations
  , class CollectNamedOperationsRL
  , collectNamedOperationsRL
  , OperationEntry
  , buildOpenAPISpec
  , buildOpenAPISpec'
  , ServerObject
  , OpenAPISpec
  , ResponseHeaderObject
  , class CollectSchemas
  , collectSchemas
  , class CollectSchemasRL
  , collectSchemasRL
  , class CollectVariantSchemasRL
  , collectVariantSchemasRL
  , class CollectRouteSchemas
  , collectRouteSchemas
  , class CollectRouteSchemasRL
  , collectRouteSchemasRL
  , class CollectSchemaNames
  , class CollectSchemaNamesRL
  , class CollectVariantSchemaNames
  , class CollectRouteSchemaNames
  , class CollectRouteSchemaNameRL
  , class ValidateNoDuplicatesRL
  , class ValidateSchemaNames
  -- , class RenderCallbacks
  -- , renderCallbacks
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Identity (Identity)
import Data.Undefined.NoProblem (Opt, toMaybe, undefined)
import Yoga.Options (class Options, options, uorToMaybe, UndefinedOr)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Foreign.Object as FObject
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Data.Reflectable (class Reflectable, reflectType)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.HTTP.API.Route.Auth (BearerToken, BasicAuth, ApiKeyHeader, ApiKeyCookie, DigestAuth)
import Yoga.HTTP.API.Route.Encoding (JSON, FormData, MultipartFormData, PlainText, XML, CustomContentType, NoBody)
import Yoga.HTTP.API.Route.HeaderValue (class HeaderValueType, headerValueType)
import Yoga.HTTP.API.Route.OpenAPIMetadata (Description, Example, Format, Minimum, Maximum, Pattern, MinLength, MaxLength, Title, Nullable, Default, Deprecated, Enum, Schema, Callback, Examples, class HasDescription, description, class HasExample, example, class HasFormat, format, class HasDeprecated, deprecated, class HasMinimum, minimum, class HasMaximum, maximum, class HasPattern, pattern, class HasMinLength, minLength, class HasMaxLength, maxLength, class HasTitle, title, class HasNullable, nullable, class HasDefault, default, class HasEnum, enum, class HasExamples, examples)
import Yoga.HTTP.API.Route.RenderMethod (class RenderMethod, renderMethod)
import Prim.Boolean (True, False)
import Data.Unit (Unit)
import Yoga.HTTP.API.Route.Response (class ToResponse, class IsUnitBody)
import Yoga.HTTP.API.Route.StatusCode (class StatusCodeMap, statusCodeFor, statusCodeToString)
import Yoga.JSON (class WriteForeign, writeImpl, unsafeStringify)
import Yoga.JSON as Yoga.JSON

--------------------------------------------------------------------------------
-- Helper Functions for Building OpenAPI Objects
--------------------------------------------------------------------------------

-- | Build a schema object with all supported OpenAPI schema properties
buildSchema
  :: { type :: String
     , format :: Maybe String
     , example :: Maybe String
     , examples :: Maybe (FObject.Object Foreign)
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
     , examples :: Maybe (FObject.Object Foreign)
     }
  -> Foreign
buildParameter { name, in: inStr, required, schema, description: descMaybe, deprecated: deprecatedMaybe, examples: examplesMaybe } =
  let
    ins :: forall a. String -> a -> FObject.Object Foreign -> FObject.Object Foreign
    ins key val obj = FObject.insert key (unsafeCoerce val) obj

    insMaybe :: forall a. String -> Maybe a -> FObject.Object Foreign -> FObject.Object Foreign
    insMaybe key valMaybe obj = maybe obj (\v -> ins key v obj) valMaybe

    base = FObject.fromFoldable
      [ Tuple "name" (unsafeCoerce name)
      , Tuple "in" (unsafeCoerce inStr)
      , Tuple "required" (unsafeCoerce required)
      , Tuple "schema" schema
      ]
  in
    unsafeCoerce $ base
      # insMaybe "description" descMaybe
      # insMaybe "deprecated" deprecatedMaybe
      # insMaybe "examples" examplesMaybe

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

-- Skip BearerToken headers (they're handled as security schemes, not regular headers)
instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name BearerToken tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name BearerToken tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip BearerToken wrapped in Description
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Description desc BearerToken) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Description desc BearerToken) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip BearerToken wrapped in Example
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Example ex BearerToken) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Example ex BearerToken) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip BearerToken wrapped in Deprecated
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Deprecated BearerToken) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Deprecated BearerToken) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip BasicAuth headers (they're handled as security schemes, not regular headers)
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name BasicAuth tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name BasicAuth tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip BasicAuth wrapped in Description
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Description desc BasicAuth) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Description desc BasicAuth) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip BasicAuth wrapped in Example
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Example ex BasicAuth) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Example ex BasicAuth) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip BasicAuth wrapped in Deprecated
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Deprecated BasicAuth) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Deprecated BasicAuth) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip ApiKeyHeader headers (they're handled as security schemes, not regular headers)
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name ApiKeyHeader tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name ApiKeyHeader tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip ApiKeyHeader wrapped in Description
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Description desc ApiKeyHeader) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Description desc ApiKeyHeader) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip ApiKeyHeader wrapped in Example
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Example ex ApiKeyHeader) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Example ex ApiKeyHeader) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip ApiKeyHeader wrapped in Deprecated
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Deprecated ApiKeyHeader) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Deprecated ApiKeyHeader) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip DigestAuth headers (they're handled as security schemes, not regular headers)
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name DigestAuth tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name DigestAuth tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip DigestAuth wrapped in Description
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Description desc DigestAuth) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Description desc DigestAuth) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip DigestAuth wrapped in Example
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Example ex DigestAuth) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Example ex DigestAuth) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Skip DigestAuth wrapped in Deprecated
else instance (RenderHeadersSchemaRL tail tailRow, Row.Cons name (Deprecated DigestAuth) tailRow headers, Row.Lacks name tailRow) => RenderHeadersSchemaRL (RL.Cons name (Deprecated DigestAuth) tail) headers where
  renderHeadersSchemaRL _ = renderHeadersSchemaRL (Proxy :: Proxy tail)

-- Required header (non-Maybe type)
else instance
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
  , HasExamples ty
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
        , examples: Nothing
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
        , examples: examples p

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

-- Case: BasicAuth found
else instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name BasicAuth tail) where
  detectSecurityRL _ =
    let
      basicAuth = unsafeCoerce $ FObject.singleton "basicAuth" ([] :: Array String)
    in
      [ basicAuth ]

-- Case: BasicAuth wrapped in Description
else instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name (Description desc BasicAuth) tail) where
  detectSecurityRL _ =
    let
      basicAuth = unsafeCoerce $ FObject.singleton "basicAuth" ([] :: Array String)
    in
      [ basicAuth ]

-- Case: BasicAuth wrapped in Example
else instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name (Example ex BasicAuth) tail) where
  detectSecurityRL _ =
    let
      basicAuth = unsafeCoerce $ FObject.singleton "basicAuth" ([] :: Array String)
    in
      [ basicAuth ]

-- Case: BasicAuth wrapped in Deprecated
else instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name (Deprecated BasicAuth) tail) where
  detectSecurityRL _ =
    let
      basicAuth = unsafeCoerce $ FObject.singleton "basicAuth" ([] :: Array String)
    in
      [ basicAuth ]

-- Case: ApiKeyHeader found
else instance (DetectSecurityRL tail, IsSymbol name) => DetectSecurityRL (RL.Cons name ApiKeyHeader tail) where
  detectSecurityRL _ =
    let
      keyName = reflectSymbol (Proxy :: Proxy name)
      apiKeyAuth = unsafeCoerce $ FObject.singleton (keyName <> "ApiKey") ([] :: Array String)
    in
      [ apiKeyAuth ]

-- Case: ApiKeyHeader wrapped in Description
else instance (DetectSecurityRL tail, IsSymbol name) => DetectSecurityRL (RL.Cons name (Description desc ApiKeyHeader) tail) where
  detectSecurityRL _ =
    let
      keyName = reflectSymbol (Proxy :: Proxy name)
      apiKeyAuth = unsafeCoerce $ FObject.singleton (keyName <> "ApiKey") ([] :: Array String)
    in
      [ apiKeyAuth ]

-- Case: ApiKeyHeader wrapped in Example
else instance (DetectSecurityRL tail, IsSymbol name) => DetectSecurityRL (RL.Cons name (Example ex ApiKeyHeader) tail) where
  detectSecurityRL _ =
    let
      keyName = reflectSymbol (Proxy :: Proxy name)
      apiKeyAuth = unsafeCoerce $ FObject.singleton (keyName <> "ApiKey") ([] :: Array String)
    in
      [ apiKeyAuth ]

-- Case: ApiKeyHeader wrapped in Deprecated
else instance (DetectSecurityRL tail, IsSymbol name) => DetectSecurityRL (RL.Cons name (Deprecated ApiKeyHeader) tail) where
  detectSecurityRL _ =
    let
      keyName = reflectSymbol (Proxy :: Proxy name)
      apiKeyAuth = unsafeCoerce $ FObject.singleton (keyName <> "ApiKey") ([] :: Array String)
    in
      [ apiKeyAuth ]

-- Case: DigestAuth found
else instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name DigestAuth tail) where
  detectSecurityRL _ =
    let
      digestAuth = unsafeCoerce $ FObject.singleton "digestAuth" ([] :: Array String)
    in
      [ digestAuth ]

-- Case: DigestAuth wrapped in Description
else instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name (Description desc DigestAuth) tail) where
  detectSecurityRL _ =
    let
      digestAuth = unsafeCoerce $ FObject.singleton "digestAuth" ([] :: Array String)
    in
      [ digestAuth ]

-- Case: DigestAuth wrapped in Example
else instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name (Example ex DigestAuth) tail) where
  detectSecurityRL _ =
    let
      digestAuth = unsafeCoerce $ FObject.singleton "digestAuth" ([] :: Array String)
    in
      [ digestAuth ]

-- Case: DigestAuth wrapped in Deprecated
else instance DetectSecurityRL tail => DetectSecurityRL (RL.Cons name (Deprecated DigestAuth) tail) where
  detectSecurityRL _ =
    let
      digestAuth = unsafeCoerce $ FObject.singleton "digestAuth" ([] :: Array String)
    in
      [ digestAuth ]

-- Case: Non-security header, recurse
else instance (DetectSecurityRL tail, HeaderValueType ty) => DetectSecurityRL (RL.Cons name ty tail) where
  detectSecurityRL _ = detectSecurityRL (Proxy :: Proxy tail)

--------------------------------------------------------------------------------
-- Security Detection for Cookies
--------------------------------------------------------------------------------

-- | Detect security requirements in request cookies
-- | Returns an array of security requirement objects (e.g., [{ sessionIdCookie: [] }])
class DetectCookieSecurity (cookies :: Row Type) where
  detectCookieSecurity :: Proxy cookies -> Array Foreign

instance (RowToList cookies rl, DetectCookieSecurityRL rl) => DetectCookieSecurity cookies where
  detectCookieSecurity _ = detectCookieSecurityRL (Proxy :: Proxy rl)

-- | Helper class using RowList to find security-related cookies
class DetectCookieSecurityRL (rl :: RowList Type) where
  detectCookieSecurityRL :: Proxy rl -> Array Foreign

-- Base case: no cookies = no security
instance DetectCookieSecurityRL RL.Nil where
  detectCookieSecurityRL _ = []

-- Case: ApiKeyCookie found
instance (DetectCookieSecurityRL tail, IsSymbol name) => DetectCookieSecurityRL (RL.Cons name ApiKeyCookie tail) where
  detectCookieSecurityRL _ =
    let
      cookieName = reflectSymbol (Proxy :: Proxy name)
      apiKeyCookie = unsafeCoerce $ FObject.singleton (cookieName <> "Cookie") ([] :: Array String)
    in
      [ apiKeyCookie ]

-- Case: ApiKeyCookie wrapped in Description
else instance (DetectCookieSecurityRL tail, IsSymbol name) => DetectCookieSecurityRL (RL.Cons name (Description desc ApiKeyCookie) tail) where
  detectCookieSecurityRL _ =
    let
      cookieName = reflectSymbol (Proxy :: Proxy name)
      apiKeyCookie = unsafeCoerce $ FObject.singleton (cookieName <> "Cookie") ([] :: Array String)
    in
      [ apiKeyCookie ]

-- Case: ApiKeyCookie wrapped in Example
else instance (DetectCookieSecurityRL tail, IsSymbol name) => DetectCookieSecurityRL (RL.Cons name (Example ex ApiKeyCookie) tail) where
  detectCookieSecurityRL _ =
    let
      cookieName = reflectSymbol (Proxy :: Proxy name)
      apiKeyCookie = unsafeCoerce $ FObject.singleton (cookieName <> "Cookie") ([] :: Array String)
    in
      [ apiKeyCookie ]

-- Case: ApiKeyCookie wrapped in Deprecated
else instance (DetectCookieSecurityRL tail, IsSymbol name) => DetectCookieSecurityRL (RL.Cons name (Deprecated ApiKeyCookie) tail) where
  detectCookieSecurityRL _ =
    let
      cookieName = reflectSymbol (Proxy :: Proxy name)
      apiKeyCookie = unsafeCoerce $ FObject.singleton (cookieName <> "Cookie") ([] :: Array String)
    in
      [ apiKeyCookie ]

-- Case: Non-security cookie, recurse
else instance (DetectCookieSecurityRL tail, HeaderValueType ty) => DetectCookieSecurityRL (RL.Cons name ty tail) where
  detectCookieSecurityRL _ = detectCookieSecurityRL (Proxy :: Proxy tail)

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
  , HasExamples ty
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
        , examples: Nothing
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
        , examples: examples p

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
  , HasExamples ty
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
        , examples: Nothing
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
        , examples: examples p

        }
      rest = renderQueryParamsSchemaRL (Proxy :: Proxy tail)
    in
      [ param ] <> rest

--------------------------------------------------------------------------------
-- Cookie Parameters Schema Generation
--------------------------------------------------------------------------------

-- | Render cookie parameters row as OpenAPI parameter array
class RenderCookieParamsSchema (params :: Row Type) where
  renderCookieParamsSchema :: Proxy params -> Array Foreign

instance (RowToList params rl, RenderCookieParamsSchemaRL rl params) => RenderCookieParamsSchema params where
  renderCookieParamsSchema _ = renderCookieParamsSchemaRL (Proxy :: Proxy rl)

-- | Helper class using RowList
class RenderCookieParamsSchemaRL (rl :: RowList Type) (params :: Row Type) | rl -> params where
  renderCookieParamsSchemaRL :: Proxy rl -> Array Foreign

instance RenderCookieParamsSchemaRL RL.Nil () where
  renderCookieParamsSchemaRL _ = []

-- Skip ApiKeyCookie cookies (they're handled as security schemes, not regular cookies)
instance (RenderCookieParamsSchemaRL tail tailRow, Row.Cons name ApiKeyCookie tailRow params, Row.Lacks name tailRow) => RenderCookieParamsSchemaRL (RL.Cons name ApiKeyCookie tail) params where
  renderCookieParamsSchemaRL _ = renderCookieParamsSchemaRL (Proxy :: Proxy tail)

-- Skip ApiKeyCookie wrapped in Description
else instance (RenderCookieParamsSchemaRL tail tailRow, Row.Cons name (Description desc ApiKeyCookie) tailRow params, Row.Lacks name tailRow) => RenderCookieParamsSchemaRL (RL.Cons name (Description desc ApiKeyCookie) tail) params where
  renderCookieParamsSchemaRL _ = renderCookieParamsSchemaRL (Proxy :: Proxy tail)

-- Skip ApiKeyCookie wrapped in Example
else instance (RenderCookieParamsSchemaRL tail tailRow, Row.Cons name (Example ex ApiKeyCookie) tailRow params, Row.Lacks name tailRow) => RenderCookieParamsSchemaRL (RL.Cons name (Example ex ApiKeyCookie) tail) params where
  renderCookieParamsSchemaRL _ = renderCookieParamsSchemaRL (Proxy :: Proxy tail)

-- Skip ApiKeyCookie wrapped in Deprecated
else instance (RenderCookieParamsSchemaRL tail tailRow, Row.Cons name (Deprecated ApiKeyCookie) tailRow params, Row.Lacks name tailRow) => RenderCookieParamsSchemaRL (RL.Cons name (Deprecated ApiKeyCookie) tail) params where
  renderCookieParamsSchemaRL _ = renderCookieParamsSchemaRL (Proxy :: Proxy tail)

-- Cookie parameter instance
else instance
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
  , HasExamples ty
  , RenderCookieParamsSchemaRL tail tailRow
  , Row.Cons name ty tailRow params
  , Row.Lacks name tailRow
  ) =>
  RenderCookieParamsSchemaRL (RL.Cons name ty tail) params where
  renderCookieParamsSchemaRL _ =
    let
      p = Proxy :: Proxy ty
      schema = buildSchema
        { type: headerValueType p
        , format: format p
        , example: example p
        , examples: Nothing
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
        , in: "cookie"
        , required: false
        , schema
        , description: description p
        , deprecated: if deprecated p then Just true else Nothing
        , examples: examples p

        }
      rest = renderCookieParamsSchemaRL (Proxy :: Proxy tail)
    in
      [ param ] <> rest

--------------------------------------------------------------------------------
-- JSON Schema Generation (Type Introspection)
--------------------------------------------------------------------------------

-- | Render a PureScript type as an OpenAPI JSON schema
class RenderJSONSchema :: forall k. k -> Constraint
class RenderJSONSchema ty where
  renderJSONSchema :: Proxy ty -> Foreign

-- Primitive types
instance RenderJSONSchema String where
  renderJSONSchema _ = buildSchema
    { type: "string"
    , format: Nothing
    , examples: Nothing
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
    , examples: Nothing
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
    , examples: Nothing
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
    , examples: Nothing
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
    , examples: Nothing
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

-- MultipartFormData encoding wrapper (unwrap and render inner type)
instance RenderJSONSchema a => RenderJSONSchema (MultipartFormData a) where
  renderJSONSchema _ = renderJSONSchema (Proxy :: Proxy a)

-- PlainText encoding wrapper (renders as string)
instance RenderJSONSchema (PlainText) where
  renderJSONSchema _ = unsafeCoerce { type: "string" }

-- XML encoding wrapper (unwrap and render inner type)
instance RenderJSONSchema a => RenderJSONSchema (XML a) where
  renderJSONSchema _ = renderJSONSchema (Proxy :: Proxy a)

-- CustomContentType encoding wrapper (unwrap and render inner type)
instance RenderJSONSchema a => RenderJSONSchema (CustomContentType mime a) where
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

-- Examples wrapper: render inner type (examples are handled elsewhere)
instance RenderJSONSchema a => RenderJSONSchema (Examples examplesRow a) where
  renderJSONSchema _ = renderJSONSchema (Proxy :: Proxy a)

-- Schema wrapper: generates $ref instead of inline schema
instance (IsSymbol name, RenderJSONSchema a) => RenderJSONSchema (Schema name a) where
  renderJSONSchema _ =
    let
      schemaName = reflectSymbol (Proxy :: Proxy name)
    in
      unsafeCoerce $ FObject.singleton "$ref" (unsafeCoerce $ "#/components/schemas/" <> schemaName)

instance (RenderJSONSchema a, Reflectable v Int) => RenderJSONSchema (MinLength v a) where
  renderJSONSchema _ = do
    let innerObj = unsafeCoerce (renderJSONSchema (Proxy :: Proxy a)) :: FObject.Object Foreign
    unsafeCoerce $ FObject.insert "minLength" (unsafeCoerce (reflectType (Proxy :: Proxy v) :: Int)) innerObj

instance (RenderJSONSchema a, Reflectable v Int) => RenderJSONSchema (MaxLength v a) where
  renderJSONSchema _ = do
    let innerObj = unsafeCoerce (renderJSONSchema (Proxy :: Proxy a)) :: FObject.Object Foreign
    unsafeCoerce $ FObject.insert "maxLength" (unsafeCoerce (reflectType (Proxy :: Proxy v) :: Int)) innerObj

instance (RenderJSONSchema a, Reflectable v Int) => RenderJSONSchema (Minimum v a) where
  renderJSONSchema _ = do
    let innerObj = unsafeCoerce (renderJSONSchema (Proxy :: Proxy a)) :: FObject.Object Foreign
    unsafeCoerce $ FObject.insert "minimum" (unsafeCoerce (reflectType (Proxy :: Proxy v) :: Int)) innerObj

instance (RenderJSONSchema a, Reflectable v Int) => RenderJSONSchema (Maximum v a) where
  renderJSONSchema _ = do
    let innerObj = unsafeCoerce (renderJSONSchema (Proxy :: Proxy a)) :: FObject.Object Foreign
    unsafeCoerce $ FObject.insert "maximum" (unsafeCoerce (reflectType (Proxy :: Proxy v) :: Int)) innerObj

instance (RenderJSONSchema a, IsSymbol pat) => RenderJSONSchema (Pattern pat a) where
  renderJSONSchema _ = do
    let innerObj = unsafeCoerce (renderJSONSchema (Proxy :: Proxy a)) :: FObject.Object Foreign
    let patStr = reflectSymbol (Proxy :: Proxy pat)
    unsafeCoerce $ if patStr == "" then innerObj
      else FObject.insert "pattern" (unsafeCoerce patStr) innerObj

instance (RenderJSONSchema a, IsSymbol t) => RenderJSONSchema (Title t a) where
  renderJSONSchema _ = do
    let innerObj = unsafeCoerce (renderJSONSchema (Proxy :: Proxy a)) :: FObject.Object Foreign
    let titleStr = reflectSymbol (Proxy :: Proxy t)
    unsafeCoerce $ if titleStr == "" then innerObj
      else FObject.insert "title" (unsafeCoerce titleStr) innerObj

instance (RenderJSONSchema a, IsSymbol val) => RenderJSONSchema (Default val a) where
  renderJSONSchema _ = do
    let innerObj = unsafeCoerce (renderJSONSchema (Proxy :: Proxy a)) :: FObject.Object Foreign
    let defStr = reflectSymbol (Proxy :: Proxy val)
    unsafeCoerce $ if defStr == "" then innerObj
      else FObject.insert "default" (unsafeCoerce defStr) innerObj

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
-- Content Type Extraction
--------------------------------------------------------------------------------

-- | Extract MIME type from encoding wrapper types
class GetContentType (encoding :: Type) where
  getContentType :: Proxy encoding -> String

instance GetContentType NoBody where
  getContentType _ = ""

instance GetContentType (JSON a) where
  getContentType _ = "application/json"

instance GetContentType (FormData a) where
  getContentType _ = "application/x-www-form-urlencoded"

instance GetContentType (MultipartFormData a) where
  getContentType _ = "multipart/form-data"

instance GetContentType (PlainText) where
  getContentType _ = "text/plain"

instance GetContentType (XML a) where
  getContentType _ = "application/xml"

instance IsSymbol mime => GetContentType (CustomContentType mime a) where
  getContentType _ = reflectSymbol (Proxy :: Proxy mime)

-- For unwrapped types (backward compatibility), default to JSON
instance GetContentType String where
  getContentType _ = "application/json"

instance GetContentType Int where
  getContentType _ = "application/json"

instance GetContentType Number where
  getContentType _ = "application/json"

instance GetContentType Boolean where
  getContentType _ = "application/json"

instance GetContentType Unit where
  getContentType _ = "application/json"

instance GetContentType a => GetContentType (Array a) where
  getContentType _ = "application/json"

instance GetContentType a => GetContentType (Maybe a) where
  getContentType _ = "application/json"

instance GetContentType (Record row) where
  getContentType _ = "application/json"

-- Metadata wrappers: pass through to inner type
instance GetContentType a => GetContentType (Description desc a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (Example ex a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (Format fmt a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (Nullable a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (Deprecated a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (Enum a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (Schema name a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (Minimum v a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (Maximum v a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (Pattern pat a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (MinLength v a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (MaxLength v a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (Title t a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

instance GetContentType a => GetContentType (Default val a) where
  getContentType _ = getContentType (Proxy :: Proxy a)

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

-- MultipartFormData: request body with multipart/form-data content type
instance RenderJSONSchema a => RenderRequestBodySchema (MultipartFormData a) where
  renderRequestBodySchema _ = Just
    { required: true
    , content: unsafeCoerce
        { "multipart/form-data":
            { schema: renderJSONSchema (Proxy :: Proxy a) }
        }
    }

-- PlainText: request body with text/plain content type
instance RenderRequestBodySchema (PlainText) where
  renderRequestBodySchema _ = Just
    { required: true
    , content: unsafeCoerce
        { "text/plain":
            { schema: unsafeCoerce { type: "string" } }
        }
    }

-- XML: request body with application/xml content type
instance RenderJSONSchema a => RenderRequestBodySchema (XML a) where
  renderRequestBodySchema _ = Just
    { required: true
    , content: unsafeCoerce
        { "application/xml":
            { schema: renderJSONSchema (Proxy :: Proxy a) }
        }
    }

-- CustomContentType: request body with custom MIME type
instance (IsSymbol mime, RenderJSONSchema a) => RenderRequestBodySchema (CustomContentType mime a) where
  renderRequestBodySchema _ =
    let
      mimeType = reflectSymbol (Proxy :: Proxy mime)
      contentObj = FObject.singleton mimeType (unsafeCoerce { schema: renderJSONSchema (Proxy :: Proxy a) })
    in
      Just { required: true, content: unsafeCoerce contentObj }

-- Record: bare record treated as application/json
instance RenderJSONSchema (Record row) => RenderRequestBodySchema (Record row) where
  renderRequestBodySchema _ = Just
    { required: true
    , content: unsafeCoerce
        { "application/json":
            { schema: renderJSONSchema (Proxy :: Proxy (Record row)) }
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
           , content :: Foreign
           }
       }

instance (RenderResponseHeadersSchema headers, RenderJSONSchema body, GetContentType body) => RenderResponseSchema headers body where
  renderResponseSchema headersProxy bodyProxy =
    let
      headers = renderResponseHeadersSchema headersProxy
      bodySchema = renderJSONSchema bodyProxy
      contentType = getContentType bodyProxy
      contentObj = FObject.singleton contentType (unsafeCoerce { schema: bodySchema })
    in
      { "200":
          { description: "Successful response"
          , headers: headers
          , content: unsafeCoerce contentObj
          }
      }

--------------------------------------------------------------------------------
-- Variant Response Schema Generation
--------------------------------------------------------------------------------

-- | Type alias for OpenAPI response object
type ResponseObject =
  { description :: String
  , headers :: FObject.Object ResponseHeaderObject
  , content :: Foreign
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
  , IsUnitBody body isUnit
  , RenderResponseContent isUnit headers body
  , RenderResponseHeadersSchema headers
  , RenderVariantResponseSchemaRL tail
  ) =>
  RenderVariantResponseSchemaRL (Cons label recordType tail) where
  renderVariantResponseSchemaRL _ =
    let
      statusCode = statusCodeFor (Proxy :: Proxy label)
      statusCodeStr = statusCodeToString statusCode
      responseObj = renderResponseContent (Proxy :: Proxy isUnit) (Proxy :: Proxy headers) (Proxy :: Proxy body)
      rest = renderVariantResponseSchemaRL (Proxy :: Proxy tail)
    in
      FObject.insert statusCodeStr responseObj rest

class RenderResponseContent (isUnit :: Boolean) (headers :: Row Type) (body :: Type) where
  renderResponseContent :: Proxy isUnit -> Proxy headers -> Proxy body -> ResponseObject

-- Unit body: no content field
instance RenderResponseHeadersSchema headers =>
  RenderResponseContent True headers Unit where
  renderResponseContent _ _ _ =
    { description: "No content"
    , headers: renderResponseHeadersSchema (Proxy :: Proxy headers)
    , content: unsafeCoerce (FObject.empty :: FObject.Object Foreign)
    }

-- Non-Unit body: render content schema
else instance (RenderResponseHeadersSchema headers, RenderJSONSchema body, GetContentType body) =>
  RenderResponseContent False headers body where
  renderResponseContent _ _ _ = do
    let headersObj = renderResponseHeadersSchema (Proxy :: Proxy headers)
    let bodySchema = renderJSONSchema (Proxy :: Proxy body)
    let contentType = getContentType (Proxy :: Proxy body)
    let contentObj = FObject.singleton contentType (unsafeCoerce { schema: bodySchema })
    { description: "Successful response"
    , headers: headersObj
    , content: unsafeCoerce contentObj
    }

-- --------------------------------------------------------------------------------
-- -- RenderCallbacks: Render full callback objects for OpenAPI
-- --------------------------------------------------------------------------------

-- | Render callbacks with full method, requestBody, and responses information.
-- | This typeclass walks through Callback wrappers and renders complete callback objects.
class RenderCallbacks (ty :: Type) where
  renderCallbacks :: Proxy ty -> FObject.Object Foreign

-- Callback wrapper: render this callback and recurse
instance
  ( IsSymbol name
  , IsSymbol expression
  , RenderMethod method
  , RenderRequestBodySchema requestBody
  , RowToList responseRow rl
  , RenderVariantResponseSchemaRL rl
  , RenderCallbacks inner
  ) =>
  RenderCallbacks (Callback inner name expression method requestBody responseRow) where
  renderCallbacks _ =
    let
      callbackName = reflectSymbol (Proxy :: Proxy name)
      callbackExpr = reflectSymbol (Proxy :: Proxy expression)
      callbackMethodStr = String.toLower (renderMethod (Proxy :: Proxy method))

      -- Build request body if present
      reqBodyObj = case renderRequestBodySchema (Proxy :: Proxy requestBody) of
        Nothing -> FObject.empty
        Just { required, content } ->
          FObject.fromFoldable
            [ Tuple "required" (unsafeCoerce required)
            , Tuple "content" content
            ]

      -- Build responses
      respSchemas = renderVariantResponseSchemaRL (Proxy :: Proxy rl)

      -- Build operation object for this callback
      operationObj = FObject.fromFoldable $
        [ Tuple "responses" (unsafeCoerce respSchemas) ] <>
          (if FObject.isEmpty reqBodyObj then [] else [ Tuple "requestBody" (unsafeCoerce reqBodyObj) ])

      -- Build the callback structure: { expression: { method: operation } }
      methodObj = FObject.singleton callbackMethodStr (unsafeCoerce operationObj)
      expressionObj = FObject.singleton callbackExpr (unsafeCoerce methodObj)

      -- Get inner callbacks
      innerCallbacks = renderCallbacks (Proxy :: Proxy inner)
    in
      FObject.insert callbackName (unsafeCoerce expressionObj) innerCallbacks

-- Base case: no callbacks
else instance RenderCallbacks ty where
  renderCallbacks _ = FObject.empty

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
-- CollectSchemas: Collect all Schema wrappers for component extraction
--------------------------------------------------------------------------------

-- | Collects all Schema wrappers found in a type and returns their definitions
class CollectSchemas (ty :: Type) where
  collectSchemas :: Proxy ty -> FObject.Object Foreign

-- Base cases: primitives don't contain schemas
instance CollectSchemas String where
  collectSchemas _ = FObject.empty

instance CollectSchemas Int where
  collectSchemas _ = FObject.empty

instance CollectSchemas Number where
  collectSchemas _ = FObject.empty

instance CollectSchemas Boolean where
  collectSchemas _ = FObject.empty

instance CollectSchemas Unit where
  collectSchemas _ = FObject.empty

-- Array: recurse into item type
instance CollectSchemas a => CollectSchemas (Array a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

-- Maybe: recurse into inner type
instance CollectSchemas a => CollectSchemas (Maybe a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

-- Record: recurse through all fields
instance (RowToList row rl, CollectSchemasRL rl) => CollectSchemas (Record row) where
  collectSchemas _ = collectSchemasRL (Proxy :: Proxy rl)

-- JSON wrapper: unwrap and recurse
instance CollectSchemas a => CollectSchemas (JSON a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

-- FormData wrapper: unwrap and recurse
instance CollectSchemas a => CollectSchemas (FormData a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

-- MultipartFormData wrapper: unwrap and recurse
instance CollectSchemas a => CollectSchemas (MultipartFormData a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

-- PlainText wrapper: no nested schemas to collect
instance CollectSchemas (PlainText) where
  collectSchemas _ = FObject.empty

-- XML wrapper: unwrap and recurse
instance CollectSchemas a => CollectSchemas (XML a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

-- CustomContentType wrapper: unwrap and recurse
instance CollectSchemas a => CollectSchemas (CustomContentType mime a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

-- NoBody: no schemas
instance CollectSchemas NoBody where
  collectSchemas _ = FObject.empty

-- Schema wrapper: COLLECT THIS + recurse into inner type
instance (IsSymbol name, RenderJSONSchema inner, CollectSchemas inner) => CollectSchemas (Schema name inner) where
  collectSchemas _ =
    let
      schemaName = reflectSymbol (Proxy :: Proxy name)
      -- Render the INNER type (not the Schema wrapper) for the component definition
      innerSchema = renderJSONSchema (Proxy :: Proxy inner)
      -- Also collect any nested schemas
      nestedSchemas = collectSchemas (Proxy :: Proxy inner)
    in
      FObject.insert schemaName innerSchema nestedSchemas

-- Metadata wrappers: recurse through to find schemas
instance CollectSchemas a => CollectSchemas (Description desc a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (Example ex a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (Format fmt a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (Minimum v a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (Maximum v a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (Pattern pat a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (MinLength v a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (MaxLength v a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (Title t a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (Nullable a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (Default val a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (Deprecated a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

instance CollectSchemas a => CollectSchemas (Enum a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

-- Examples wrapper: recurse through to find schemas
instance CollectSchemas a => CollectSchemas (Examples examplesRow a) where
  collectSchemas _ = collectSchemas (Proxy :: Proxy a)

-- | Helper class to collect schemas from record fields
class CollectSchemasRL (rl :: RowList Type) where
  collectSchemasRL :: Proxy rl -> FObject.Object Foreign

instance CollectSchemasRL Nil where
  collectSchemasRL _ = FObject.empty

instance (IsSymbol name, CollectSchemas ty, CollectSchemasRL tail) => CollectSchemasRL (Cons name ty tail) where
  collectSchemasRL _ =
    let
      fieldSchemas = collectSchemas (Proxy :: Proxy ty)
      restSchemas = collectSchemasRL (Proxy :: Proxy tail)
    in
      FObject.union fieldSchemas restSchemas

-- | Collects schemas from all response bodies in a variant
class CollectVariantSchemasRL (rl :: RowList Type) where
  collectVariantSchemasRL :: Proxy rl -> FObject.Object Foreign

instance CollectVariantSchemasRL Nil where
  collectVariantSchemasRL _ = FObject.empty

instance
  ( ToResponse recordType headers body
  , CollectSchemas body
  , CollectVariantSchemasRL tail
  ) =>
  CollectVariantSchemasRL (Cons label recordType tail) where
  collectVariantSchemasRL _ =
    let
      bodySchemas = collectSchemas (Proxy :: Proxy body)
      restSchemas = collectVariantSchemasRL (Proxy :: Proxy tail)
    in
      FObject.union bodySchemas restSchemas

-- | Collects schemas from a route's request and response types
class CollectRouteSchemas (route :: Type) where
  collectRouteSchemas :: Proxy route -> FObject.Object Foreign

-- | Helper class for collecting schemas from a record of routes
class CollectRouteSchemasRL (rl :: RowList Type) where
  collectRouteSchemasRL :: Proxy rl -> FObject.Object Foreign

instance CollectRouteSchemasRL Nil where
  collectRouteSchemasRL _ = FObject.empty

instance
  ( CollectRouteSchemas routeType
  , CollectRouteSchemasRL tail
  ) =>
  CollectRouteSchemasRL (Cons label routeType tail) where
  collectRouteSchemasRL _ =
    let
      routeSchemas = collectRouteSchemas (Proxy :: Proxy routeType)
      restSchemas = collectRouteSchemasRL (Proxy :: Proxy tail)
    in
      FObject.union routeSchemas restSchemas

-- Record instance: iterate record fields via RowList
instance (RowToList row rl, CollectRouteSchemasRL rl) => CollectRouteSchemas (Record row) where
  collectRouteSchemas _ = collectRouteSchemasRL (Proxy :: Proxy rl)

--------------------------------------------------------------------------------
-- CollectSchemaNames: Type-level collision detection
--------------------------------------------------------------------------------

-- | Collects schema names at the type level as a Row.
-- | Used with Row.Union + Row.Nub to detect name collisions at compile time.
-- | Two Schema wrappers with the same name but different types cause a compile error.
class CollectSchemaNames (ty :: Type) (names :: Row Type) | ty -> names

instance CollectSchemaNames String ()
instance CollectSchemaNames Int ()
instance CollectSchemaNames Number ()
instance CollectSchemaNames Boolean ()
instance CollectSchemaNames Unit ()

instance CollectSchemaNames a names => CollectSchemaNames (Array a) names
instance CollectSchemaNames a names => CollectSchemaNames (Maybe a) names

instance (RowToList row rl, CollectSchemaNamesRL rl names) => CollectSchemaNames (Record row) names

instance CollectSchemaNames a names => CollectSchemaNames (JSON a) names
instance CollectSchemaNames a names => CollectSchemaNames (FormData a) names
instance CollectSchemaNames a names => CollectSchemaNames (MultipartFormData a) names
instance CollectSchemaNames (PlainText) ()
instance CollectSchemaNames a names => CollectSchemaNames (XML a) names
instance CollectSchemaNames a names => CollectSchemaNames (CustomContentType mime a) names
instance CollectSchemaNames NoBody ()

instance
  ( CollectSchemaNames inner innerNames
  , Row.Cons name inner innerNames names
  , Row.Lacks name innerNames
  ) =>
  CollectSchemaNames (Schema name inner) names

-- Metadata wrappers: recurse through
instance CollectSchemaNames a names => CollectSchemaNames (Description desc a) names
instance CollectSchemaNames a names => CollectSchemaNames (Example ex a) names
instance CollectSchemaNames a names => CollectSchemaNames (Format fmt a) names
instance CollectSchemaNames a names => CollectSchemaNames (Minimum v a) names
instance CollectSchemaNames a names => CollectSchemaNames (Maximum v a) names
instance CollectSchemaNames a names => CollectSchemaNames (Pattern pat a) names
instance CollectSchemaNames a names => CollectSchemaNames (MinLength v a) names
instance CollectSchemaNames a names => CollectSchemaNames (MaxLength v a) names
instance CollectSchemaNames a names => CollectSchemaNames (Title t a) names
instance CollectSchemaNames a names => CollectSchemaNames (Nullable a) names
instance CollectSchemaNames a names => CollectSchemaNames (Default val a) names
instance CollectSchemaNames a names => CollectSchemaNames (Deprecated a) names
instance CollectSchemaNames a names => CollectSchemaNames (Enum a) names
instance CollectSchemaNames a names => CollectSchemaNames (Examples examplesRow a) names

-- | Helper class to collect schema names from record fields
class CollectSchemaNamesRL (rl :: RowList Type) (names :: Row Type) | rl -> names

instance CollectSchemaNamesRL Nil ()

instance
  ( CollectSchemaNames ty fieldNames
  , CollectSchemaNamesRL tail tailNames
  , Row.Union fieldNames tailNames names
  ) =>
  CollectSchemaNamesRL (Cons name ty tail) names

-- | Collects schema names from all response bodies in a variant
class CollectVariantSchemaNames (rl :: RowList Type) (names :: Row Type) | rl -> names

instance CollectVariantSchemaNames Nil ()

instance
  ( ToResponse recordType headers body
  , CollectSchemaNames body bodyNames
  , CollectVariantSchemaNames tail tailNames
  , Row.Union bodyNames tailNames names
  ) =>
  CollectVariantSchemaNames (Cons label recordType tail) names

-- | Collects schema names from a route's request and response types
class CollectRouteSchemaNames (route :: Type) (names :: Row Type) | route -> names

-- | Helper for collecting schema names from a record of routes
class CollectRouteSchemaNameRL (rl :: RowList Type) (names :: Row Type) | rl -> names

instance CollectRouteSchemaNameRL Nil ()

instance
  ( CollectRouteSchemaNames routeType routeNames
  , CollectRouteSchemaNameRL tail tailNames
  , Row.Union routeNames tailNames names
  ) =>
  CollectRouteSchemaNameRL (Cons label routeType tail) names

-- Record instance
instance (RowToList row rl, CollectRouteSchemaNameRL rl names) => CollectRouteSchemaNames (Record row) names

-- | Walks a RowList and checks each entry against the full row.
-- | If the RowList has duplicate labels from Union, Cons will unify their types.
-- | Same name + same type: Cons succeeds. Same name + different type: Cons fails.
class ValidateNoDuplicatesRL (rl :: RowList Type) (full :: Row Type)

instance ValidateNoDuplicatesRL Nil full

instance
  ( Row.Cons name ty _rest full
  , ValidateNoDuplicatesRL tail full
  ) =>
  ValidateNoDuplicatesRL (Cons name ty tail) full

-- | Validates that all Schema wrappers in a route collection have unique names.
-- | Same name with same type is allowed (deduplication).
-- | Same name with different types is a compile error.
class ValidateSchemaNames (routes :: Type)

instance
  ( CollectRouteSchemaNames routes names
  , RowToList names rl
  , ValidateNoDuplicatesRL rl names
  ) =>
  ValidateSchemaNames routes

--------------------------------------------------------------------------------
-- CollectOperations: Walk a type-level structure of routes
--------------------------------------------------------------------------------

type OperationEntry =
  { method :: String
  , path :: String
  , operation :: String
  }

-- | Collect OpenAPI operations from a type-level structure of routes.
-- | Use a Record to combine multiple named routes:
-- |   CollectOperations { getUser :: RouteA, createUser :: RouteB }
class CollectOperations (routes :: Type) where
  collectOperations :: Proxy routes -> Array OperationEntry

-- Note: Route instance is defined in Route.purs to avoid import cycles

--------------------------------------------------------------------------------
-- CollectNamedOperationsRL: Walk a record RowList and inject operationId
--------------------------------------------------------------------------------

class CollectNamedOperationsRL (rl :: RowList Type) where
  collectNamedOperationsRL :: Proxy rl -> Array OperationEntry

instance CollectNamedOperationsRL Nil where
  collectNamedOperationsRL _ = []

instance
  ( IsSymbol label
  , CollectOperations routeType
  , CollectNamedOperationsRL tail
  ) =>
  CollectNamedOperationsRL (Cons label routeType tail) where
  collectNamedOperationsRL _ = do
    let
      name = reflectSymbol (Proxy :: Proxy label)
      entries = collectOperations (Proxy :: Proxy routeType)
      namedEntries = entries <#> \entry ->
        entry { operation = setOperationId name entry.operation }
      rest = collectNamedOperationsRL (Proxy :: Proxy tail)
    namedEntries <> rest

-- Record instance: iterate record fields via RowList
instance (RowToList row rl, CollectNamedOperationsRL rl) => CollectOperations (Record row) where
  collectOperations _ = collectNamedOperationsRL (Proxy :: Proxy rl)

--------------------------------------------------------------------------------
-- buildOpenAPISpec: Assemble a complete OpenAPI 3.0 document
--------------------------------------------------------------------------------

-- | Opaque type representing a complete OpenAPI specification document.
foreign import data OpenAPISpec :: Type

instance WriteForeign OpenAPISpec where
  writeImpl = unsafeCoerce

-- | Detect which security schemes are used across all operations
-- | and build the securitySchemes component
buildSecuritySchemes :: Array OperationEntry -> FObject.Object Foreign
buildSecuritySchemes ops =
  let
    securityKeys = ops # foldl collectSecurityKeys []
      where
      collectSecurityKeys acc entry =
        let
          opObj = unsafeCoerce (unsafeParseJSON entry.operation) :: FObject.Object Foreign
          securityField = FObject.lookup "security" opObj
        in
          case securityField of
            Nothing -> acc
            Just secArray ->
              let
                secList = unsafeCoerce secArray :: Array (FObject.Object Foreign)
                keys = secList >>= FObject.keys
              in
                acc <> keys

    uniqueKeys = Array.nub securityKeys
    schemes = uniqueKeys <#> \key ->
      let
        scheme = case key of
          "bearerAuth" -> Just $ Tuple key $ unsafeCoerce
            { type: "http"
            , scheme: "bearer"
            , bearerFormat: "JWT"
            }
          "basicAuth" -> Just $ Tuple key $ unsafeCoerce
            { type: "http"
            , scheme: "basic"
            }
          "digestAuth" -> Just $ Tuple key $ unsafeCoerce
            { type: "http"
            , scheme: "digest"
            }
          _ ->
            -- API Key in cookie (names ending with "Cookie")
            if String.take (String.length key - 6) key /= "" && String.drop (String.length key - 6) key == "Cookie" then
              let
                cookieName = String.take (String.length key - 6) key
              in
                Just $ Tuple key $ unsafeCoerce
                  { type: "apiKey"
                  , in: "cookie"
                  , name: cookieName
                  }
            -- API Key in header (names ending with "ApiKey")
            else if String.take (String.length key - 6) key /= "" && String.drop (String.length key - 6) key == "ApiKey" then
              let
                headerName = String.take (String.length key - 6) key
              in
                Just $ Tuple key $ unsafeCoerce
                  { type: "apiKey"
                  , in: "header"
                  , name: headerName
                  }
            else Nothing
      in
        scheme
  in
    FObject.fromFoldable $ Array.catMaybes schemes

-- | Type for OpenAPI server object
type ServerObject =
  { url :: String
  , description :: Maybe String
  }

-- | Row type for contact information (all fields will be coerced)
type ContactInfoR =
  ( name :: String
  , url :: String
  , email :: String
  )

-- | Contact info record
type ContactInfo = { | ContactInfoR }

-- | Row type for license information
type LicenseInfoR =
  ( name :: String
  , url :: String
  )

-- | License info record
type LicenseInfo = { | LicenseInfoR }

-- | Row type for OpenAPI info object
type OpenAPIInfoR f =
  ( title :: String
  , version :: String
  , description :: f String
  , contact :: f ContactInfo
  , license :: f LicenseInfo
  )

-- | OpenAPI info with all required fields
type OpenAPIInfo = { | OpenAPIInfoR Identity }

-- | OpenAPI info with optional fields (using UndefinedOr)
type OpenAPIInfoUor = { | OpenAPIInfoR UndefinedOr }

buildOpenAPISpec
  :: forall @routes r
   . CollectOperations routes
  => CollectRouteSchemas routes
  => ValidateSchemaNames routes
  => Options r (OpenAPIInfoR UndefinedOr)
  => { | r }
  -> OpenAPISpec
buildOpenAPISpec given = buildOpenAPISpec' @routes (options @(OpenAPIInfoR UndefinedOr) given) { servers: undefined }

-- | Build a complete OpenAPI 3.0 spec with optional servers configuration.
buildOpenAPISpec'
  :: forall @routes
   . CollectOperations routes
  => CollectRouteSchemas routes
  => ValidateSchemaNames routes
  => OpenAPIInfoUor
  -> { servers :: Opt (Array ServerObject) }
  -> OpenAPISpec
buildOpenAPISpec' info config =
  let
    ops = collectOperations (Proxy :: Proxy routes)
    paths = groupByPath ops
    securitySchemes = buildSecuritySchemes ops
    schemas = collectRouteSchemas (Proxy :: Proxy routes)

    -- Build components object with both schemas and securitySchemes
    components =
      if FObject.isEmpty schemas && FObject.isEmpty securitySchemes then Nothing
      else Just $ unsafeCoerce $ FObject.fromFoldable $
        (if FObject.isEmpty schemas then [] else [ Tuple "schemas" (unsafeCoerce schemas) ])
          <> (if FObject.isEmpty securitySchemes then [] else [ Tuple "securitySchemes" (unsafeCoerce securitySchemes) ])

    -- Build info object with optional fields
    infoBase = FObject.fromFoldable
      [ Tuple "title" (writeImpl info.title)
      , Tuple "version" (writeImpl info.version)
      ]
    infoWithDescription = maybe infoBase (\desc -> FObject.insert "description" (writeImpl desc) infoBase) (uorToMaybe info.description)
    infoWithContact = maybe infoWithDescription
      ( \contact ->
          FObject.insert "contact" (writeImpl contact) infoWithDescription
      )
      (uorToMaybe info.contact)
    infoWithLicense = maybe infoWithContact
      ( \license ->
          FObject.insert "license" (writeImpl license) infoWithContact
      )
      (uorToMaybe info.license)

    baseSpec = FObject.fromFoldable
      [ Tuple "openapi" (unsafeCoerce "3.0.0")
      , Tuple "info" (unsafeCoerce infoWithLicense)
      , Tuple "paths" (unsafeCoerce paths)
      ]
    withComponents = maybe baseSpec (\c -> FObject.insert "components" c baseSpec) components
    withServers = maybe withComponents (\servers -> FObject.insert "servers" (writeImpl servers) withComponents) (toMaybe config.servers)
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

unsafeParseJSON :: String -> Foreign
unsafeParseJSON str = case Yoga.JSON.readJSON_ str of
  Just (obj :: FObject.Object Foreign) -> unsafeCoerce obj
  Nothing -> unsafeCoerce str

stripKeys :: Array String -> Foreign -> Foreign
stripKeys keys obj = do
  let fobj = (unsafeCoerce :: Foreign -> FObject.Object Foreign) obj
  unsafeCoerce $ foldl (flip FObject.delete) fobj keys

setOperationId :: String -> String -> String
setOperationId name jsonStr = case Yoga.JSON.readJSON_ jsonStr of
  Nothing -> jsonStr
  Just (obj :: FObject.Object Foreign) ->
    Yoga.JSON.unsafeStringify $ FObject.insert "operationId" (unsafeCoerce name) obj
