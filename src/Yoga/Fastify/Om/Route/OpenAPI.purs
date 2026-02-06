module Yoga.Fastify.Om.Route.OpenAPI
  ( class RenderHeadersSchema
  , renderHeadersSchema
  , class RenderHeadersSchemaRL
  , renderHeadersSchemaRL
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
  , class ToOpenAPI
  , toOpenAPIImpl
  , toOpenAPI
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object as FObject
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Om.Route.HeaderValue (class HeaderValueType, headerValueType)
import Yoga.Fastify.Om.Route.Response (class ToResponse)
import Yoga.Fastify.Om.Route.StatusCode (class StatusCodeMap, statusCodeFor, statusCodeToString)

--------------------------------------------------------------------------------
-- OpenAPI Generation
--------------------------------------------------------------------------------

-- | Render headers row as OpenAPI parameter array
class RenderHeadersSchema (headers :: Row Type) where
  renderHeadersSchema :: Proxy headers -> Array { name :: String, in :: String, required :: Boolean, schema :: { type :: String } }

instance (RowToList headers rl, RenderHeadersSchemaRL rl headers) => RenderHeadersSchema headers where
  renderHeadersSchema _ = renderHeadersSchemaRL (Proxy :: Proxy rl)

-- | Helper class using RowList
class RenderHeadersSchemaRL (rl :: RowList Type) (headers :: Row Type) | rl -> headers where
  renderHeadersSchemaRL :: Proxy rl -> Array { name :: String, in :: String, required :: Boolean, schema :: { type :: String } }

instance RenderHeadersSchemaRL RL.Nil () where
  renderHeadersSchemaRL _ = []

-- Required header (non-Maybe type)
instance
  ( IsSymbol name
  , HeaderValueType ty
  , RenderHeadersSchemaRL tail tailRow
  , Row.Cons name ty tailRow headers
  , Row.Lacks name tailRow
  ) =>
  RenderHeadersSchemaRL (RL.Cons name ty tail) headers where
  renderHeadersSchemaRL _ =
    let
      headerName = reflectSymbol (Proxy :: Proxy name)
      headerType = headerValueType (Proxy :: Proxy ty)
      param = { name: headerName, in: "header", required: true, schema: { type: headerType } }
      rest = renderHeadersSchemaRL (Proxy :: Proxy tail)
    in
      [ param ] <> rest

-- Optional header (Maybe type) - need a separate instance
-- This won't work with current instance heads, so we'll treat Maybe headers as required for now
-- In a real implementation, you'd use instance chains or fundeps to distinguish

--------------------------------------------------------------------------------
-- Response Headers Schema Generation
--------------------------------------------------------------------------------

-- | Render response headers row as OpenAPI header object (for responses section)
class RenderResponseHeadersSchema (headers :: Row Type) where
  renderResponseHeadersSchema :: Proxy headers -> FObject.Object { schema :: { type :: String } }

instance (RowToList headers rl, RenderResponseHeadersSchemaRL rl headers) => RenderResponseHeadersSchema headers where
  renderResponseHeadersSchema _ = renderResponseHeadersSchemaRL (Proxy :: Proxy rl)

-- | Helper class using RowList
class RenderResponseHeadersSchemaRL (rl :: RowList Type) (headers :: Row Type) | rl -> headers where
  renderResponseHeadersSchemaRL :: Proxy rl -> FObject.Object { schema :: { type :: String } }

instance RenderResponseHeadersSchemaRL RL.Nil () where
  renderResponseHeadersSchemaRL _ = FObject.empty

-- Response header instance
instance
  ( IsSymbol name
  , HeaderValueType ty
  , RenderResponseHeadersSchemaRL tail tailRow
  , Row.Cons name ty tailRow headers
  , Row.Lacks name tailRow
  ) =>
  RenderResponseHeadersSchemaRL (RL.Cons name ty tail) headers where
  renderResponseHeadersSchemaRL _ =
    let
      headerName = reflectSymbol (Proxy :: Proxy name)
      headerType = headerValueType (Proxy :: Proxy ty)
      header = { schema: { type: headerType } }
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
           , headers :: FObject.Object { schema :: { type :: String } }
           , content ::
               { "application/json" ::
                   { schema :: { type :: String } }
               }
           }
       }

instance (RenderResponseHeadersSchema headers) => RenderResponseSchema headers body where
  renderResponseSchema headersProxy _ =
    let
      headers = renderResponseHeadersSchema headersProxy
    in
      { "200":
          { description: "Successful response"
          , headers: headers
          , content:
              { "application/json":
                  { schema: { type: "object" } }
              }
          }
      }

--------------------------------------------------------------------------------
-- Variant Response Schema Generation
--------------------------------------------------------------------------------

-- | Type alias for OpenAPI response object
type ResponseObject =
  { description :: String
  , headers :: FObject.Object { schema :: { type :: String } }
  , content :: { "application/json" :: { schema :: { type :: String } } }
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
  , RenderVariantResponseSchemaRL tail
  ) =>
  RenderVariantResponseSchemaRL (Cons label recordType tail) where
  renderVariantResponseSchemaRL _ =
    let
      statusCode = statusCodeFor (Proxy :: Proxy label)
      statusCodeStr = statusCodeToString statusCode
      headersObj = renderResponseHeadersSchema (Proxy :: Proxy headers)
      responseObj =
        { description: "Successful response"
        , headers: headersObj
        , content:
            { "application/json":
                { schema: { type: "object" } }
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
