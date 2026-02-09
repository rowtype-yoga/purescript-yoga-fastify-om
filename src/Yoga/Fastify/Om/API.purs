module Yoga.Fastify.Om.API
  ( registerAPI
  , registerAPILayer
  , class RegisterAPI
  , class RegisterAPIRL
  , registerAPIRL
  , class RegisterHandler
  , registerHandler
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Class (liftEffect)
import Prim.Row as Row
import Prim.RowList as RL
import Prim.RowList (class RowToList)
import Record as Record
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Fastify (Fastify)
import Yoga.Fastify.Route.HandleResponse (class HandleResponse)
import Yoga.Fastify.Route.HandleRoute (handleRoute)
import Yoga.Fastify.Route.ParseBody (class ParseBody)
import Yoga.Fastify.Route.ParseHeaders (class ParseHeaders)
import Yoga.Fastify.Route.ParsePathParams (class ParsePathParams)
import Yoga.Fastify.Route.ParseQueryParams (class ParseQueryParamsFromObject)
import Yoga.HTTP.API.Path (class PathPattern)
import Yoga.HTTP.API.Route.Handler (Request, class DefaultRequestFields, class SegmentPathParams, class SegmentQueryParams, class EncodingBody)
import Yoga.HTTP.API.Route.RenderMethod (class RenderMethod)
import Yoga.HTTP.API.Route.Route (Route, class ConvertResponseVariant)
import Yoga.HTTP.API.Route.RouteHandler (Handler, class RouteHandler)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Om (Om)
import Yoga.Om.Layer (OmLayer, makeLayer)

class RegisterHandler handler where
  registerHandler :: handler -> Fastify -> Effect Unit

instance
  ( Row.Union partialRequest o_ (headers :: Record fullHeaders, cookies :: Record fullCookies, body :: fullEncoding)
  , DefaultRequestFields partialRequest fullHeaders fullCookies fullEncoding
  , RenderMethod method
  , PathPattern segments
  , SegmentPathParams segments pathParams
  , SegmentQueryParams segments queryParams
  , EncodingBody fullEncoding body
  , ParsePathParams pathParams
  , ParseQueryParamsFromObject queryParams
  , ParseHeaders fullHeaders
  , ParseBody fullEncoding body
  , ConvertResponseVariant userResp respVariant
  , HandleResponse respVariant
  , RouteHandler (Route method segments (Request (Record partialRequest)) userResp) pathParams queryParams fullHeaders body respVariant
  ) =>
  RegisterHandler (Handler (Route method segments (Request (Record partialRequest)) userResp)) where
  registerHandler = handleRoute
else instance
  RegisterHandler (Handler (Route method segments (Request (Record partialRequest)) userResp)) =>
  RegisterHandler (Handler (Route method segments (Record partialRequest) userResp)) where
  registerHandler handler = registerHandler (asRequest handler)
    where
    asRequest :: Handler (Route method segments (Record partialRequest) userResp) -> Handler (Route method segments (Request (Record partialRequest)) userResp)
    asRequest = unsafeCoerce

class RegisterAPI (handlers :: Row Type) where
  registerAPI :: Record handlers -> Fastify -> Effect Unit

instance
  ( RowToList handlers rl
  , RegisterAPIRL rl handlers
  ) =>
  RegisterAPI handlers where
  registerAPI handlers fastify = registerAPIRL (Proxy :: Proxy rl) handlers fastify

class RegisterAPIRL (rl :: RL.RowList Type) (handlers :: Row Type) where
  registerAPIRL :: Proxy rl -> Record handlers -> Fastify -> Effect Unit

instance RegisterAPIRL RL.Nil handlers where
  registerAPIRL _ _ _ = pure unit

instance
  ( IsSymbol label
  , Row.Cons label handler rest handlers
  , RegisterHandler handler
  , RegisterAPIRL tail handlers
  ) =>
  RegisterAPIRL (RL.Cons label handler tail) handlers where
  registerAPIRL _ handlers fastify = do
    registerHandler (Record.get (Proxy :: Proxy label) handlers) fastify
    registerAPIRL (Proxy :: Proxy tail) handlers fastify

registerAPILayer
  :: forall handlers ctx err
   . RegisterAPI handlers
  => Record handlers
  -> OmLayer (fastify :: Fastify | ctx) () err
registerAPILayer handlers = makeLayer do
  { fastify } <- ask
  registerAPI handlers fastify # liftEffect
  pure {}
