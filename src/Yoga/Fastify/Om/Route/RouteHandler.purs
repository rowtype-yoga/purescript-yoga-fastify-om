module Yoga.Fastify.Om.Route.RouteHandler
  ( Handler
  , class RouteHandler
  , mkHandler
  , runHandler
  ) where

import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Fastify.Om.Route.Handler
  ( HandlerFn
  , Request
  , class DefaultRequestFields
  , class EncodingBody
  , class SegmentPathParams
  , class SegmentQueryParams
  )
import Yoga.Fastify.Om.Route.Route (Route)

-- | Type class that computes the handler function type from a Route type.
class
  RouteHandler
    (route :: Type)
    (pathParams :: Row Type)
    (queryParams :: Row Type)
    (reqHeaders :: Row Type)
    (body :: Type)
    (respVariant :: Row Type)
  | route -> pathParams queryParams reqHeaders body respVariant

instance
  ( Row.Union partialRequest o_ (headers :: Record fullHeaders, body :: fullEncoding)
  , DefaultRequestFields partialRequest fullHeaders fullEncoding
  , SegmentPathParams segments pathParams
  , SegmentQueryParams segments queryParams
  , EncodingBody fullEncoding body
  ) =>
  RouteHandler
    (Route method segments (Request partialRequest) respVariant)
    pathParams
    queryParams
    fullHeaders
    body
    respVariant

-- | A handler tied to a specific route type.
-- |
-- | Usage:
-- |   userHandler :: Handler UserRoute
-- |   userHandler = mkHandler \{ path } -> ...
foreign import data Handler :: Type -> Type

-- | Create a Handler from a function matching the route's type.
mkHandler
  :: forall route pathParams queryParams reqHeaders body respVariant
   . RouteHandler route pathParams queryParams reqHeaders body respVariant
  => HandlerFn pathParams queryParams reqHeaders body respVariant
  -> Handler route
mkHandler = unsafeCoerce

-- | Extract the handler function from a Handler.
runHandler
  :: forall route pathParams queryParams reqHeaders body respVariant
   . RouteHandler route pathParams queryParams reqHeaders body respVariant
  => Handler route
  -> HandlerFn pathParams queryParams reqHeaders body respVariant
runHandler = unsafeCoerce
