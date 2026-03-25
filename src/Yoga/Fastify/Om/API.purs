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
  , class ResolveHandlerCtx
  , class ResolveHandlerCtxRL
  , class CheckHandlerDependency
  , class ValidateAPIHandlers
  , class CheckAPIHandler
  , class NoExtraHandlers
  , class CheckAPIFieldExists
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
import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
import Record as Record
import Type.Function (type ($))
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

class ResolveHandlers (ctx :: Row Type) (handlers :: Row Type) (resolved :: Row Type) | ctx handlers -> resolved where
  resolveHandlers :: Record ctx -> Record handlers -> Record resolved

instance
  ( RL.RowToList handlers rl
  , ResolveHandlersRL rl ctx handlers resolved
  ) =>
  ResolveHandlers ctx handlers resolved where
  resolveHandlers ctx handlers = Builder.buildFromScratch (resolveHandlersRL (Proxy :: Proxy rl) ctx handlers)

class ResolveHandlersRL (rl :: RL.RowList Type) (ctx :: Row Type) (handlers :: Row Type) (resolved :: Row Type) | rl handlers -> resolved where
  resolveHandlersRL :: Proxy rl -> Record ctx -> Record handlers -> Builder (Record ()) (Record resolved)

instance ResolveHandlersRL RL.Nil ctx handlers () where
  resolveHandlersRL _ _ _ = identity

class ResolveHandlerCtx (apiLabel :: Symbol) (handlerCtx :: Row Type) (ctx :: Row Type)

instance
  ( RL.RowToList handlerCtx handlerCtxRL
  , RL.RowToList ctx ctxRL
  , ResolveHandlerCtxRL apiLabel handlerCtx handlerCtxRL ctx ctxRL
  ) =>
  ResolveHandlerCtx apiLabel handlerCtx ctx

class ResolveHandlerCtxRL (apiLabel :: Symbol) (handlerCtx :: Row Type) (required :: RL.RowList Type) (ctx :: Row Type) (available :: RL.RowList Type)

instance ResolveHandlerCtxRL apiLabel handlerCtx RL.Nil ctx available

instance
  ( CheckHandlerDependency apiLabel label ty available handlerCtx ctx
  , ResolveHandlerCtxRL apiLabel handlerCtx tail ctx available
  ) =>
  ResolveHandlerCtxRL apiLabel handlerCtx (RL.Cons label ty tail) ctx available

class CheckHandlerDependency (apiLabel :: Symbol) (label :: Symbol) (ty :: Type) (available :: RL.RowList Type) (handlerCtx :: Row Type) (ctx :: Row Type)

instance
  CheckHandlerDependency apiLabel label ty (RL.Cons label ty tail) handlerCtx ctx
else instance
  ( IsSymbol apiLabel
  , IsSymbol label
  , Fail (
      Above
        (Text "registerAPILayer dependency type mismatch.") $
        Above
          (Beside (Text "Handler: ") (Quote apiLabel)) $
          Above
            (Beside (Text "Dependency: ") (Quote label)) $
            Above
              (Beside (Text "Expected: ") (Quote ty)) $
              Above
                (Beside (Text "Actual: ") (Quote actualTy)) $
                Above
                  (Beside (Text "Handler context: ") (Quote handlerCtx))
                  (Beside (Text "Layer context: ") (Quote ctx))
      )
  ) =>
  CheckHandlerDependency apiLabel label ty (RL.Cons label actualTy tail) handlerCtx ctx
else instance
  CheckHandlerDependency apiLabel label ty tail handlerCtx ctx =>
  CheckHandlerDependency apiLabel label ty (RL.Cons otherLabel otherTy tail) handlerCtx ctx
else instance
  ( IsSymbol apiLabel
  , IsSymbol label
  , Fail (
      Above
        (Text "registerAPILayer is missing a required dependency.") $
        Above
          (Beside (Text "Handler: ") (Quote apiLabel)) $
          Above
            (Beside (Text "Missing dependency: ") (Quote label)) $
            Above
              (Beside (Text "Expected type: ") (Quote ty)) $
              Above
                (Beside (Text "Handler context: ") (Quote handlerCtx))
                (Beside (Text "Layer context: ") (Quote ctx))
      )
  ) =>
  CheckHandlerDependency apiLabel label ty RL.Nil handlerCtx ctx

instance
  ( IsSymbol label
  , Row.Cons label (Handler route handlerCtx) rest handlers
  , ResolveHandlerCtx label handlerCtx ctx
  , ResolveHandlersRL tail ctx handlers tailResolved
  , Row.Cons label (Internal.Handler route) tailResolved resolved
  , Row.Lacks label tailResolved
  ) =>
  ResolveHandlersRL (RL.Cons label (Handler route handlerCtx) tail) ctx handlers resolved where
  resolveHandlersRL _ ctx handlers =
    resolveHandlersRL (Proxy :: Proxy tail) ctx handlers
      >>> Builder.insert (Proxy :: Proxy label) handler
    where
    Handler mkHandler = Record.get (Proxy :: Proxy label) handlers
    handler = mkHandler (unsafeCoerce ctx)

class ValidateAPIHandlers (apiRL :: RL.RowList Type) (handlerRL :: RL.RowList Type) (handlers :: Row Type)

instance ValidateAPIHandlers RL.Nil handlerRL handlers

instance
  ( CheckAPIHandler label route handlerRL
  , ValidateAPIHandlers tail handlerRL handlers
  ) =>
  ValidateAPIHandlers (RL.Cons label route tail) handlerRL handlers

class CheckAPIHandler (label :: Symbol) (route :: Type) (handlerRL :: RL.RowList Type)

instance
  CheckAPIHandler label route (RL.Cons label (Handler route handlerCtx) tail)
else instance
  ( IsSymbol label
  , Fail (
      Above
        (Text "registerAPILayer handler route mismatch.") $
        Above
          (Beside (Text "Handler field: ") (Quote label)) $
          Above
            (Beside (Text "Expected route: ") (Quote route))
            (Beside (Text "Actual handler type: ") (Quote handler))
      )
  ) =>
  CheckAPIHandler label route (RL.Cons label handler tail)
else instance
  CheckAPIHandler label route tail =>
  CheckAPIHandler label route (RL.Cons otherLabel handler tail)
else instance
  ( IsSymbol label
  , Fail (
      Above
        (Text "registerAPILayer is missing a handler.") $
        Above
          (Beside (Text "Missing field: ") (Quote label))
          (Beside (Text "Expected route: ") (Quote route))
      )
  ) =>
  CheckAPIHandler label route RL.Nil

class NoExtraHandlers (handlerRL :: RL.RowList Type) (apiRL :: RL.RowList Type) (apiRow :: Row Type)

instance NoExtraHandlers RL.Nil apiRL apiRow

instance
  ( CheckAPIFieldExists label apiRL apiRow
  , NoExtraHandlers tail apiRL apiRow
  ) =>
  NoExtraHandlers (RL.Cons label handler tail) apiRL apiRow

class CheckAPIFieldExists (label :: Symbol) (apiRL :: RL.RowList Type) (apiRow :: Row Type)

instance CheckAPIFieldExists label (RL.Cons label route tail) apiRow
else instance
  CheckAPIFieldExists label tail apiRow =>
  CheckAPIFieldExists label (RL.Cons otherLabel route tail) apiRow
else instance
  ( IsSymbol label
  , Fail (
      Above
        (Text "registerAPILayer received an extra handler.") $
        Above
          (Beside (Text "Extra field: ") (Quote label))
          (Beside (Text "API shape: ") (Quote apiRow))
      )
  ) =>
  CheckAPIFieldExists label RL.Nil apiRow

class APIHandlers (rl :: RL.RowList Type) (handlers :: Row Type) | rl -> handlers

instance APIHandlers RL.Nil ()

instance
  ( APIHandlers tail tailHandlers
  , Row.Cons label (Handler route handlerCtx) tailHandlers handlers
  , Row.Lacks label tailHandlers
  ) =>
  APIHandlers (RL.Cons label route tail) handlers

registerAPILayer
  :: forall @api apiRow apiRL handlers handlerRL resolved ctx
   . ApiRecord api apiRow
  => RL.RowToList apiRow apiRL
  => RL.RowToList handlers handlerRL
  => ValidateAPIHandlers apiRL handlerRL handlers
  => NoExtraHandlers handlerRL apiRL apiRow
  => ResolveHandlers (fastify :: Fastify | ctx) handlers resolved
  => RegisterAPI resolved
  => Record handlers
  -> OmLayer (fastify :: Fastify | ctx) () {}
registerAPILayer handlers = makeLayer do
  ctx <- ask
  let resolved = resolveHandlers ctx handlers
  registerAPI resolved (unsafeCoerce ctx :: { fastify :: Fastify }).fastify # liftEffect
  pure {}
