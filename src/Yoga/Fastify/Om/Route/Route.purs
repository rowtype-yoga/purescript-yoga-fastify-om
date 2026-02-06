module Yoga.Fastify.Om.Route.Route
  ( Route(..)
  , route
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
import Yoga.Fastify.Om.Path (class PathPattern, pathPattern)
import Yoga.Fastify.Om.Route.Handler (Request, class DefaultRequestFields, class SegmentPathParams, class SegmentQueryParams)
import Yoga.Fastify.Om.Route.OpenAPI (class CollectOperations, class RenderHeadersSchema, renderHeadersSchema, class RenderPathParamsSchema, renderPathParamsSchema, class RenderQueryParamsSchema, renderQueryParamsSchema, class RenderRequestBodySchema, renderRequestBodySchema, class RenderVariantResponseSchemaRL, renderVariantResponseSchemaRL, class DetectSecurity, detectSecurity, class ToOpenAPI, toOpenAPIImpl)
import Yoga.Fastify.Om.Route.OpenAPIMetadata (class HasOperationMetadata, operationMetadata)
import Yoga.Fastify.Om.Route.RenderMethod (class RenderMethod, renderMethod)
import Yoga.Fastify.Om.Route.Response (Response, class ToResponse)
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

-- | Smart constructor for Route that allows partial request records.
-- |
-- | Users can specify only the fields they need:
-- |   route (Proxy :: _ GET) (Proxy :: _ path) (Proxy :: _ (Request {}))  -- no headers or body
-- |   route (Proxy :: _ GET) (Proxy :: _ path) (Proxy :: _ (Request { body :: JSON User }))  -- only body
-- |   route (Proxy :: _ GET) (Proxy :: _ path) (Proxy :: _ (Request { headers :: { auth :: String } }))  -- only headers
route
  :: forall method segments partialRequest o_ fullHeaders fullBody userRespVariant internalRespVariant
   . Union partialRequest o_ (headers :: fullHeaders, body :: fullBody)
  => ConvertResponseVariant userRespVariant internalRespVariant
  => Proxy method
  -> Proxy segments
  -> Proxy (Request (Record partialRequest))
  -> Proxy userRespVariant
  -> Route method segments (Request (Record partialRequest)) userRespVariant
route _ _ _ _ = Route

instance
  ( RenderMethod method
  , PathPattern segments
  , DefaultRequestFields partialRequest reqHeaders encoding
  , RenderHeadersSchema reqHeaders
  , DetectSecurity reqHeaders
  , SegmentPathParams segments pathParams
  , RenderPathParamsSchema pathParams
  , SegmentQueryParams segments queryParams
  , RenderQueryParamsSchema queryParams
  , RenderRequestBodySchema encoding
  , RowToList userResp rl
  , RenderVariantResponseSchemaRL rl
  , HasOperationMetadata (Route method segments (Request (Record partialRequest)) userResp)
  ) =>
  ToOpenAPI (Route method segments (Request (Record partialRequest)) userResp) where
  toOpenAPIImpl proxy =
    let
      methodStr = renderMethod (Proxy :: Proxy method)
      pathStr = pathPattern (Proxy :: Proxy segments)
      headerParams = renderHeadersSchema (Proxy :: Proxy reqHeaders)
      pathParams = renderPathParamsSchema (Proxy :: Proxy pathParams)
      queryParams = renderQueryParamsSchema (Proxy :: Proxy queryParams)
      parameters = headerParams <> pathParams <> queryParams
      requestBody = renderRequestBodySchema (Proxy :: Proxy encoding)
      responses = renderVariantResponseSchemaRL (Proxy :: Proxy rl)
      security = detectSecurity (Proxy :: Proxy reqHeaders)
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
