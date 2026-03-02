module Yoga.HTTP.API.Route.Route
  ( Route(..)
  , class ConvertResponseVariant
  , class ConvertResponseVariantRL
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Path (class PathPattern, pathPattern)
import Yoga.HTTP.API.Route.Handler (Request, class DefaultRequestFields, class SegmentPathParams, class SegmentQueryParams)
import Yoga.HTTP.API.Route.OpenAPI (class CollectOperations, class RenderHeadersSchema, renderHeadersSchema, class RenderCookieParamsSchema, renderCookieParamsSchema, class RenderPathParamsSchema, renderPathParamsSchema, class RenderQueryParamsSchema, renderQueryParamsSchema, class RenderRequestBodySchema, renderRequestBodySchema, class RenderVariantResponseSchemaRL, renderVariantResponseSchemaRL, class DetectSecurity, detectSecurity, class DetectCookieSecurity, detectCookieSecurity, class ToOpenAPI, toOpenAPIImpl, class CollectSchemas, collectSchemas, class CollectVariantSchemasRL, collectVariantSchemasRL, class CollectRouteSchemas, collectRouteSchemas, class CollectSchemaNames, class CollectVariantSchemaNames, class CollectRouteSchemaNames)
import Yoga.HTTP.API.Route.OpenAPIMetadata (class HasOperationMetadata, operationMetadata)
import Yoga.HTTP.API.Route.RenderMethod (class RenderMethod, renderMethod)
import Yoga.HTTP.API.Route.Response (Response, class ToResponse)
import Foreign.Object as FObject
import Yoga.JSON (writeJSON)

data Route :: forall k. Type -> k -> Type -> Row Type -> Type
data Route method segments request respVariant = Route

--------------------------------------------------------------------------------
-- ConvertResponseVariant: Convert record syntax to Response types in variant
--------------------------------------------------------------------------------

-- | Convert a variant row with record syntax to Response types.
-- | Input: ( ok :: { body :: User }, notFound :: { body :: ErrorMsg } )
-- | Output: ( ok :: Response () User, notFound :: Response () ErrorMsg )
class ConvertResponseVariant (userRow :: Row Type) (internalRow :: Row Type) | userRow -> internalRow

instance convertResponseVariantImpl ::
  ( RowToList userRow rl
  , ConvertResponseVariantRL rl () internalRow
  ) =>
  ConvertResponseVariant userRow internalRow

class ConvertResponseVariantRL (rl :: RowList Type) (acc :: Row Type) (out :: Row Type) | rl acc -> out

instance convertResponseVariantRLNil :: ConvertResponseVariantRL RL.Nil acc acc

instance convertResponseVariantRLCons ::
  ( ToResponse recordType headers body
  , ConvertResponseVariantRL tail acc1 acc2
  , Cons label (Response headers body) acc2 out
  , Lacks label acc2
  ) =>
  ConvertResponseVariantRL (RL.Cons label recordType tail) acc1 out

instance
  ( RenderMethod method
  , PathPattern segments
  , DefaultRequestFields partialRequest reqHeaders reqCookies encoding
  , RenderHeadersSchema reqHeaders
  , RenderCookieParamsSchema reqCookies
  , DetectSecurity reqHeaders
  , DetectCookieSecurity reqCookies
  , SegmentPathParams segments pathParams
  , RenderPathParamsSchema pathParams
  , SegmentQueryParams segments queryParams
  , RenderQueryParamsSchema queryParams
  , RenderRequestBodySchema encoding
  , RowToList userResp rl
  , RenderVariantResponseSchemaRL rl
  , HasOperationMetadata (Route method segments (Record partialRequest) userResp)
  ) =>
  ToOpenAPI (Route method segments (Record partialRequest) userResp) where
  toOpenAPIImpl proxy =
    let
      methodStr = renderMethod (Proxy :: Proxy method)
      pathStr = pathPattern (Proxy :: Proxy segments)
      headerParams = renderHeadersSchema (Proxy :: Proxy reqHeaders)
      cookieParams = renderCookieParamsSchema (Proxy :: Proxy reqCookies)
      pathParams = renderPathParamsSchema (Proxy :: Proxy pathParams)
      queryParams = renderQueryParamsSchema (Proxy :: Proxy queryParams)
      parameters = headerParams <> cookieParams <> pathParams <> queryParams
      requestBody = renderRequestBodySchema (Proxy :: Proxy encoding)
      responses = renderVariantResponseSchemaRL (Proxy :: Proxy rl)
      headerSecurity = detectSecurity (Proxy :: Proxy reqHeaders)
      cookieSecurity = detectCookieSecurity (Proxy :: Proxy reqCookies)
      security = headerSecurity <> cookieSecurity
      metadata = operationMetadata proxy
      operation =
        { method: methodStr
        , path: pathStr
        , parameters
        , requestBody
        , responses
        , security: if Array.length security == 0 then Nothing else Just security
        , summary: metadata.summary
        , description: metadata.description
        , operationId: metadata.operationId
        , tags: metadata.tags
        , deprecated: if metadata.deprecated then Just true else Nothing
        }
    in
      writeJSON operation

else instance
  ToOpenAPI (Route method segments (Record partialRequest) userResp) =>
  ToOpenAPI (Route method segments (Request (Record partialRequest)) userResp) where
  toOpenAPIImpl _ = toOpenAPIImpl (Proxy :: Proxy (Route method segments (Record partialRequest) userResp))
-- CollectOperations instance for Route
instance
  ( RenderMethod method
  , PathPattern segments
  , ToOpenAPI (Route method segments request resp)
  ) =>
  CollectOperations (Route method segments request resp) where
  collectOperations _ =
    [ { method: renderMethod (Proxy :: Proxy method)
      , path: pathPattern (Proxy :: Proxy segments)
      , operation: toOpenAPIImpl (Proxy :: Proxy (Route method segments request resp))
      }
    ]

-- CollectRouteSchemas instance for Route
instance
  ( DefaultRequestFields partialRequest reqHeaders reqCookies encoding
  , CollectSchemas encoding
  , RowToList userResp rl
  , CollectVariantSchemasRL rl
  ) =>
  CollectRouteSchemas (Route method segments (Record partialRequest) userResp) where
  collectRouteSchemas _ =
    let
      requestSchemas = collectSchemas (Proxy :: Proxy encoding)
      responseSchemas = collectVariantSchemasRL (Proxy :: Proxy rl)
    in
      FObject.union requestSchemas responseSchemas

else instance
  CollectRouteSchemas (Route method segments (Record partialRequest) userResp) =>
  CollectRouteSchemas (Route method segments (Request (Record partialRequest)) userResp) where
  collectRouteSchemas _ = collectRouteSchemas (Proxy :: Proxy (Route method segments (Record partialRequest) userResp))
-- CollectRouteSchemaNames instance for Route (compile-time collision detection)
instance
  ( DefaultRequestFields partialRequest reqHeaders reqCookies encoding
  , CollectSchemaNames encoding reqNames
  , RowToList userResp rl
  , CollectVariantSchemaNames rl respNames
  , Union reqNames respNames names
  ) =>
  CollectRouteSchemaNames (Route method segments (Record partialRequest) userResp) names

else instance
  CollectRouteSchemaNames (Route method segments (Record partialRequest) userResp) names =>
  CollectRouteSchemaNames (Route method segments (Request (Record partialRequest)) userResp) names