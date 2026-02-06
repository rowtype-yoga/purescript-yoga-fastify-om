module Yoga.Fastify.Om.Route.Route
  ( Route(..)
  , route
  , class ConvertResponseVariant
  , class ConvertResponseVariantRL
  ) where

import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Om.Route.Handler (Request, class DefaultRequestFields)
import Yoga.Fastify.Om.Route.OpenAPI (class RenderHeadersSchema, renderHeadersSchema, class RenderVariantResponseSchemaRL, renderVariantResponseSchemaRL, class ToOpenAPI)
import Yoga.Fastify.Om.Route.RenderMethod (class RenderMethod)
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
  , DefaultRequestFields partialRequest reqHeaders encoding
  , RenderHeadersSchema reqHeaders
  , RowToList userResp rl
  , RenderVariantResponseSchemaRL rl
  ) =>
  ToOpenAPI (Route method segments (Request (Record partialRequest)) userResp) where
  toOpenAPIImpl _ =
    let
      parameters = renderHeadersSchema (Proxy :: Proxy reqHeaders)
      responses = renderVariantResponseSchemaRL (Proxy :: Proxy rl)
      operation = { parameters, responses }
    in
      writeJSON operation
