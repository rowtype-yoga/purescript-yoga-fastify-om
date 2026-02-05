module Yoga.Fastify.Om.Route.Route
  ( Route(..)
  ) where

import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Om.Route.Handler (class RequestHeaders)
import Yoga.Fastify.Om.Route.OpenAPI (class RenderHeadersSchema, renderHeadersSchema, class RenderVariantResponseSchemaRL, renderVariantResponseSchemaRL, class ToOpenAPI)
import Yoga.Fastify.Om.Route.RenderMethod (class RenderMethod)
import Yoga.JSON (writeJSON)

data Route (method :: Type) (segments :: Type) (request :: Type) (respVariant :: Row Type) = Route

instance
  ( RenderMethod method
  , RequestHeaders request reqHeaders
  , RenderHeadersSchema reqHeaders
  , RowToList respVariantRow rl
  , RenderVariantResponseSchemaRL rl
  ) =>
  ToOpenAPI (Route method segments request respVariantRow) where
  toOpenAPIImpl _ =
    let
      parameters = renderHeadersSchema (Proxy :: Proxy reqHeaders)
      responses = renderVariantResponseSchemaRL (Proxy :: Proxy rl)
      operation = { parameters, responses }
    in
      writeJSON operation
