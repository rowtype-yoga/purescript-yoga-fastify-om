module Yoga.Fastify.Om.API
  ( registerAPI
  , registerAPILayer
  , class RegisterAPI
  , class RegisterAPIRL
  , registerAPIRL
  , class RegisterHandler
  , registerHandler
  , class ResolveHandlers
  , resolveHandlers
  , class ResolveHandlersRL
  , resolveHandlersRL
  , class APIHandlers
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
import Yoga.HTTP.API.Route.RouteHandler (class RouteHandler, class ApiRecord)
import Yoga.HTTP.API.Route.RouteHandler as Internal
import Yoga.Fastify.Om.Route.OmHandler (Handler(..))
import Record.Builder (Builder)
import Record.Builder as Builder
import Unsafe.Coerce (unsafeCoerce)
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
  RegisterHandler (Internal.Handler (Route method segments (Request (Record partialRequest)) userResp)) where
  registerHandler = handleRoute
else instance
  RegisterHandler (Internal.Handler (Route method segments (Request (Record partialRequest)) userResp)) =>
  RegisterHandler (Internal.Handler (Route method segments (Record partialRequest) userResp)) where
  registerHandler handler = registerHandler (asRequest handler)
    where
    asRequest :: Internal.Handler (Route method segments (Record partialRequest) userResp) -> Internal.Handler (Route method segments (Request (Record partialRequest)) userResp)
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
  , Row.Cons label (Handler route ctx) rest handlers
  , RegisterHandler (Internal.Handler route)
  , RegisterAPIRL tail handlers
  ) =>
  RegisterAPIRL (RL.Cons label (Handler route ctx) tail) handlers where
  registerAPIRL _ handlers fastify = do
    let Handler f = Record.get (Proxy :: Proxy label) handlers
    registerHandler (f (unsafeCoerce {})) fastify
    registerAPIRL (Proxy :: Proxy tail) handlers fastify
else instance
  ( IsSymbol label
  , Row.Cons label handler rest handlers
  , RegisterHandler handler
  , RegisterAPIRL tail handlers
  ) =>
  RegisterAPIRL (RL.Cons label handler tail) handlers where
  registerAPIRL _ handlers fastify = do
    registerHandler (Record.get (Proxy :: Proxy label) handlers) fastify
    registerAPIRL (Proxy :: Proxy tail) handlers fastify

class ResolveHandlers (handlers :: Row Type) (resolved :: Row Type) | handlers -> resolved where
  resolveHandlers :: forall ctx. Record ctx -> Record handlers -> Record resolved

instance
  ( RL.RowToList handlers rl
  , ResolveHandlersRL rl handlers resolved
  ) =>
  ResolveHandlers handlers resolved where
  resolveHandlers ctx handlers = Builder.buildFromScratch (resolveHandlersRL (Proxy :: Proxy rl) ctx handlers)

class ResolveHandlersRL (rl :: RL.RowList Type) (handlers :: Row Type) (resolved :: Row Type) | rl -> resolved where
  resolveHandlersRL :: forall ctx. Proxy rl -> Record ctx -> Record handlers -> Builder (Record ()) (Record resolved)

instance ResolveHandlersRL RL.Nil handlers () where
  resolveHandlersRL _ _ _ = identity

instance
  ( IsSymbol label
  , Row.Cons label (Handler route handlerCtx) rest handlers
  , ResolveHandlersRL tail handlers tailResolved
  , Row.Cons label (Internal.Handler route) tailResolved resolved
  , Row.Lacks label tailResolved
  ) =>
  ResolveHandlersRL (RL.Cons label (Handler route handlerCtx) tail) handlers resolved where
  resolveHandlersRL _ ctx handlers =
    resolveHandlersRL (Proxy :: Proxy tail) ctx handlers
      >>> Builder.insert (Proxy :: Proxy label) handler
    where
    Handler mkHandler = Record.get (Proxy :: Proxy label) handlers
    handler = mkHandler (unsafeCoerce ctx)

class APIHandlers (rl :: RL.RowList Type) (handlers :: Row Type) | rl -> handlers

instance APIHandlers RL.Nil ()

instance
  ( APIHandlers tail tailHandlers
  , Row.Cons label (Handler route handlerCtx) tailHandlers handlers
  , Row.Lacks label tailHandlers
  ) =>
  APIHandlers (RL.Cons label route tail) handlers

registerAPILayer
  :: forall @api apiRow apiRL handlers resolved ctx
   . ApiRecord api apiRow
  => RL.RowToList apiRow apiRL
  => APIHandlers apiRL handlers
  => ResolveHandlers handlers resolved
  => RegisterAPI resolved
  => Record handlers
  -> OmLayer (fastify :: Fastify | ctx) () {}
registerAPILayer handlers = makeLayer do
  ctx <- ask
  let resolved = resolveHandlers ctx handlers
  registerAPI resolved (unsafeCoerce ctx :: { fastify :: Fastify }).fastify # liftEffect
  pure {}
