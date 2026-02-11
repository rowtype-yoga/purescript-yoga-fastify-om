module Yoga.Fastify.Om.Route.OmHandler
  ( handle
  , Handler(..)
  , respond
  , respondWith
  , respondNoContent
  , respondNotModified
  , reject
  , rejectWith
  , mapReject
  , class ToLabel
  , class Is2xxStatus
  , class SplitResponse
  , class SplitResponseRL
  , class SplitResponseEntry
  , class BuildErrorHandlers
  , buildErrorHandlers
  , class RouteResponseVariant
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff as Aff
import Effect.Exception (Error)
import Prim.Boolean (True, False)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Route.Response (Response(..))
import Yoga.HTTP.API.Route.StatusCode (class StatusCodeToLabel)
import Yoga.HTTP.API.Route.Route (Route, class ConvertResponseVariant)
import Yoga.HTTP.API.Route.RouteHandler (class RouteHandler)
import Yoga.HTTP.API.Route.RouteHandler as Internal
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Om (Om, handleErrors', runOm)

-- | Convert either a status code (Int) or label (Symbol) to a label (Symbol).
-- | This allows functions to accept both @404 and @"notFound" polymorphically.
class ToLabel :: Type -> Symbol -> Constraint
class ToLabel labelOrCode label | labelOrCode -> label

-- Int codes map to their label via StatusCodeToLabel
instance toLabelInt :: StatusCodeToLabel code label => ToLabel (Proxy code) label

-- Symbols map to themselves
else instance toLabelSymbol :: ToLabel (Proxy label) label

-- | Determine whether a variant label corresponds to a 2xx HTTP status code.
class Is2xxStatus (label :: Symbol) (is2xx :: Boolean) | label -> is2xx

instance Is2xxStatus "ok" True
else instance Is2xxStatus "created" True
else instance Is2xxStatus "accepted" True
else instance Is2xxStatus "noContent" True
else instance Is2xxStatus label False

-- | Split a response variant row into success (2xx) and error (non-2xx) sub-rows.
class
  SplitResponse (respVariant :: Row Type) (successRow :: Row Type) (errorRow :: Row Type)
  | respVariant -> successRow errorRow

instance
  ( RL.RowToList respVariant rl
  , SplitResponseRL rl successRow errorRow
  ) =>
  SplitResponse respVariant successRow errorRow

class
  SplitResponseRL (rl :: RL.RowList Type) (successRow :: Row Type) (errorRow :: Row Type)
  | rl -> successRow errorRow

instance SplitResponseRL RL.Nil () ()

instance
  ( Is2xxStatus label is2xx
  , SplitResponseEntry is2xx label ty tail successRow errorRow
  ) =>
  SplitResponseRL (RL.Cons label ty tail) successRow errorRow

-- | Dispatch based on Is2xxStatus result.
class
  SplitResponseEntry
    (is2xx :: Boolean)
    (label :: Symbol)
    (ty :: Type)
    (tail :: RL.RowList Type)
    (successRow :: Row Type)
    (errorRow :: Row Type)
  | is2xx label ty tail -> successRow errorRow

-- 2xx: add to success row
instance
  ( SplitResponseRL tail successTail errorRow
  , Row.Cons label ty successTail successRow
  , Row.Lacks label successTail
  ) =>
  SplitResponseEntry True label ty tail successRow errorRow

-- non-2xx: add to error row
instance
  ( SplitResponseRL tail successRow errorTail
  , Row.Cons label ty errorTail errorRow
  , Row.Lacks label errorTail
  ) =>
  SplitResponseEntry False label ty tail successRow errorRow

-- | Build a record of handlers for `Variant.onMatch` that convert each
-- | error variant label into the full response variant.
class
  BuildErrorHandlers
    (rl :: RL.RowList Type)
    (respVariant :: Row Type)
    (handlers :: Row Type)
  | rl respVariant -> handlers where
  buildErrorHandlers
    :: Proxy rl
    -> Proxy respVariant
    -> Builder (Record ()) (Record handlers)

instance BuildErrorHandlers RL.Nil respVariant () where
  buildErrorHandlers _ _ = identity

instance
  ( IsSymbol label
  , Row.Cons label ty _r1 respVariant
  , BuildErrorHandlers tail respVariant tailHandlers
  , Row.Cons label (ty -> Om ctx () (Variant respVariant)) tailHandlers handlers
  , Row.Lacks label tailHandlers
  ) =>
  BuildErrorHandlers (RL.Cons label ty tail) respVariant handlers where
  buildErrorHandlers _ respProxy =
    buildErrorHandlers (Proxy :: Proxy tail) respProxy
      >>> Builder.insert (Proxy :: Proxy label) handler
    where
    handler :: ty -> Om ctx () (Variant respVariant)
    handler val = pure (Variant.inj (Proxy :: Proxy label) val)

-- | Return a response with a specific reason label or status code
-- |
-- | Example:
-- | ```purescript
-- | respond @"ok" { id: 1, name: "Alice" }
-- | respond @200 { id: 1, name: "Alice" }
-- | respond @"created" newUser
-- | respond @201 newUser
-- | ```
respond
  :: forall @labelOrCode label body r1 r2 ctx err
   . ToLabel (Proxy labelOrCode) label
  => IsSymbol label
  => Row.Cons label (Response () body) r1 r2
  => body
  -> Om ctx err (Variant r2)
respond body =
  pure (Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body }))

-- | Return a response with a specific reason label or status code and custom headers
-- |
-- | Example:
-- | ```purescript
-- | respondWith @"created" { "Location": "/users/123" } user
-- | respondWith @201 { "Location": "/users/123" } user
-- | ```
respondWith
  :: forall @labelOrCode label headers body r1 r2 ctx err
   . ToLabel (Proxy labelOrCode) label
  => IsSymbol label
  => Row.Cons label (Response headers body) r1 r2
  => Record headers
  -> body
  -> Om ctx err (Variant r2)
respondWith headers body =
  pure (Variant.inj (Proxy :: Proxy label) (Response { headers, body }))

-- | Return a 204 No Content response (no headers, no body).
-- |
-- | Example:
-- | ```purescript
-- | respondNoContent
-- | ```
respondNoContent
  :: forall ctx err r
   . Om ctx err (Variant (noContent :: Response () Unit | r))
respondNoContent =
  pure ((Variant.inj (Proxy :: Proxy "noContent") (Response { headers: {}, body: unit })))

-- | Return a 304 Not Modified response (no headers, no body).
-- |
-- | Example:
-- | ```purescript
-- | respondNotModified
-- | ```
respondNotModified
  :: forall ctx err r
   . Om ctx err (Variant (notModified :: Response () Unit | r))
respondNotModified =
  pure (Variant.inj (Proxy :: Proxy "notModified") (Response { headers: {}, body: unit }))

-- | Throw a non-2xx error response (short-circuits the handler)
-- |
-- | Example:
-- | ```purescript
-- | reject @"notFound" { error: "User not found" }
-- | reject @404 { error: "User not found" }
-- | reject @"badRequest" { error: "Invalid input" }
-- | reject @400 { error: "Invalid input" }
-- | ```
reject
  :: forall @labelOrCode label body _r1 err _r2 ctx a
   . ToLabel (Proxy labelOrCode) label
  => IsSymbol label
  => Row.Cons label (Response () body) _r1 err
  => Row.Cons label (Response () body) _r2 (exception :: Error | err)
  => body
  -> Om ctx err a
reject body =
  throwError (Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body }))

-- | Throw a non-2xx error response with custom headers (short-circuits)
-- |
-- | Example:
-- | ```purescript
-- | rejectWith @"unauthorized" { "WWW-Authenticate": "Bearer" } { error: "Invalid token" }
-- | rejectWith @401 { "WWW-Authenticate": "Bearer" } { error: "Invalid token" }
-- | ```
rejectWith
  :: forall @labelOrCode label headers body _r1 err _r2 ctx a
   . ToLabel (Proxy labelOrCode) label
  => IsSymbol label
  => Row.Cons label (Response headers body) _r1 err
  => Row.Cons label (Response headers body) _r2 (exception :: Error | err)
  => Record headers
  -> body
  -> Om ctx err a
rejectWith headers body =
  throwError (Variant.inj (Proxy :: Proxy label) (Response { headers, body }))

mapReject
  :: forall @from @toLabelOrCode toLabel tyIn body ctx errIn errMid errOut a
   . IsSymbol from
  => ToLabel (Proxy toLabelOrCode) toLabel
  => IsSymbol toLabel
  => Row.Cons from tyIn (exception :: Error | errMid) (exception :: Error | errIn)
  => Row.Cons toLabel (Response () body) (exception :: Error | errMid) (exception :: Error | errOut)
  => (tyIn -> body)
  -> Om ctx errIn a
  -> Om ctx errOut a
mapReject f = handleErrors' \variant ->
  variant # Variant.on (Proxy :: Proxy from)
    (\v -> throwError (Variant.inj (Proxy :: Proxy toLabel) (Response { headers: {}, body: f v })))
    (throwError <<< unsafeCoerce)

-- | A handler with deferred dependency injection.
-- | The `ctx` row tracks what extra dependencies are needed beyond the request.
-- | Use `handle` to create one, and `registerAPILayer` to provide the deps.
newtype Handler route (ctx :: Row Type) = Handler (Record ctx -> Internal.Handler route)

-- | Create a `Handler` from an Om computation.
-- |
-- | The Om computation receives the request context (path, query, headers, body)
-- | plus any extra dependencies via `ask`. Request field types are verified
-- | against the route at compile time.
-- |
-- | Example:
-- | ```purescript
-- | putUserHandler :: Handler PutUser (userRepo :: UserRepo)
-- | putUserHandler = handle do
-- |   { path, body, userRepo } <- ask
-- |   existing <- userRepo.findByName path.name # liftAff
-- |   case existing of
-- |     Just user -> respond @"ok" user
-- |     Nothing -> do
-- |       user <- userRepo.create path.name body.email # liftAff
-- |       respond @"created" user
-- | ```
handle
  :: forall @route pathParams queryParams reqHeaders body respVariant
       successRow errorRow extraCtx
   . RouteHandler route pathParams queryParams reqHeaders body respVariant
  => SplitResponse respVariant successRow errorRow
  => Row.Lacks "_respondNow" errorRow
  => Row.Lacks "path" extraCtx
  => Row.Lacks "query" extraCtx
  => Row.Lacks "headers" extraCtx
  => Row.Lacks "body" extraCtx
  => Om
       { path :: Record pathParams
       , query :: Record queryParams
       , headers :: Record reqHeaders
       , body :: body
       | extraCtx
       }
       (_respondNow :: Variant successRow | errorRow)
       (Variant successRow)
  -> Handler route extraCtx
handle om = Handler \deps -> unsafeCoerce \requestCtx -> do
  let ctx = unsafeMerge deps requestCtx
  runOm ctx { exception: Aff.throwError } $
    handleErrors' errorHandler (unsafeExpandVariant <$> om')
  where
  om' = handleErrors' respondNowHandler om
  respondNowHandler = Variant.on (Proxy :: Proxy "_respondNow") pure throwError
  errorHandler = Variant.on (Proxy :: Proxy "exception")
    (throwError <<< Variant.inj (Proxy :: Proxy "exception"))
    (pure <<< unsafeExpandVariant)

class RouteResponseVariant (route :: Type) (respVariant :: Row Type) | route -> respVariant

instance
  ( ConvertResponseVariant userResp respVariant
  ) =>
  RouteResponseVariant (Route method segments request userResp) respVariant

foreign import unsafeMergeImpl :: forall a b. a -> b -> a

unsafeMerge :: forall a b c. Record a -> Record b -> Record c
unsafeMerge a b = unsafeCoerce (unsafeMergeImpl a b)

unsafeExpandVariant :: forall a b. Variant a -> Variant b
unsafeExpandVariant = unsafeCoerce
