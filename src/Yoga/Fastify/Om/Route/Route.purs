module Yoga.Fastify.Om.Route.Route
  ( Route(..)
  , route
  ) where

import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Om.Route.Handler (class DefaultRequestFields)
import Yoga.Fastify.Om.Route.OpenAPI (class RenderHeadersSchema, renderHeadersSchema, class RenderVariantResponseSchemaRL, renderVariantResponseSchemaRL, class ToOpenAPI)
import Yoga.Fastify.Om.Route.RenderMethod (class RenderMethod)
import Yoga.JSON (writeJSON)

data Route (method :: Type) (segments :: Type) (request :: Type) (respVariant :: Row Type) = Route

-- | Smart constructor for Route that allows partial request records.
-- |
-- | Users can specify only the fields they need:
-- |   route (Proxy :: _ GET) (Proxy :: _ path) (Proxy :: _ (Request ()))  -- no headers or body
-- |   route (Proxy :: _ GET) (Proxy :: _ path) (Proxy :: _ (Request (body :: JSON User)))  -- only body
-- |   route (Proxy :: _ GET) (Proxy :: _ path) (Proxy :: _ (Request (headers :: { auth :: String })))  -- only headers
route
  :: forall method segments partialRequest o_ fullHeaders fullBody respVariant
   . Union partialRequest o_ (headers :: fullHeaders, body :: fullBody)
  => Proxy method
  -> Proxy segments
  -> Proxy (Record partialRequest)
  -> Proxy respVariant
  -> Route method segments (Record partialRequest) respVariant
route _ _ _ _ = Route

instance
  ( RenderMethod method
  , DefaultRequestFields partialRequest reqHeaders encoding
  , RenderHeadersSchema reqHeaders
  , RowToList respVariantRow rl
  , RenderVariantResponseSchemaRL rl
  ) =>
  ToOpenAPI (Route method segments (Record partialRequest) respVariantRow) where
  toOpenAPIImpl _ =
    let
      parameters = renderHeadersSchema (Proxy :: Proxy reqHeaders)
      responses = renderVariantResponseSchemaRL (Proxy :: Proxy rl)
      operation = { parameters, responses }
    in
      writeJSON operation
