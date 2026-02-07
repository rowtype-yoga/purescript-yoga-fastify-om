module Yoga.Fastify.Om.Route.OmHandler
  ( handle
  , respondReason
  , respondWith
  , respondNoContent
  , respondNotModified
  , reject
  , rejectWith
  , class Is2xxStatus
  , class SplitResponse
  , class SplitResponseRL
  , class SplitResponseEntry
  , class BuildErrorHandlers
  , buildErrorHandlers
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Symbol (class IsSymbol)
import Data.Variant (class VariantMatchCases, Variant)
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
import Yoga.HTTP.API.Route.RouteHandler (Handler, class RouteHandler, mkHandler)
import Yoga.Om (Om, handleErrors', runOm)

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

-- | Return a response with a specific reason label (e.g., "ok", "created")
-- |
-- | Example:
-- | ```purescript
-- | respondReason @"ok" { id: 1, name: "Alice" }
-- | respondReason @"created" newUser
-- | ```
respondReason
  :: forall @label body r1 r2 ctx err
   . IsSymbol label
  => Row.Cons label (Response () body) r1 r2
  => body
  -> Om ctx err (Variant r2)
respondReason body =
  pure (Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body }))

-- | Return a response with a specific reason label and custom headers
-- |
-- | Example:
-- | ```purescript
-- | respondWith @"created" { "Location": "/users/123" } user
-- | ```
respondWith
  :: forall @label headers body r1 r2 ctx err
   . IsSymbol label
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
-- respondNoContent
--   :: forall r1 r2 ctx err
--    . Row.Cons "noContent" (Response () Unit) r1 r2
--   => Om ctx err (Variant r2)
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
-- | reject @"badRequest" { error: "Invalid input" }
-- | ```
reject
  :: forall @label body _r1 err _r2 ctx a
   . IsSymbol label
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
-- | ```
rejectWith
  :: forall @label headers body _r1 err _r2 ctx a
   . IsSymbol label
  => Row.Cons label (Response headers body) _r1 err
  => Row.Cons label (Response headers body) _r2 (exception :: Error | err)
  => Record headers
  -> body
  -> Om ctx err a
rejectWith headers body =
  throwError (Variant.inj (Proxy :: Proxy label) (Response { headers, body }))

-- | Create a `Handler` from an Om computation.
-- |
-- | The Om computation receives the request as context (via `ask`),
-- | can short-circuit with non-2xx responses (via `throw`),
-- | and returns a 2xx response on the happy path.
-- |
-- | Example:
-- | ```purescript
-- | userHandler :: Handler UserRoute
-- | userHandler = handle do
-- |   { path } <- ask
-- |   when (path.id /= 1) $
-- |     throw { notFound: respondNoHeaders @"notFound" { error: "User not found" } }
-- |   pure $ respondNoHeaders @"ok" { id: 1, name: "Alice", email: "alice@example.com" }
-- | ```
handle
  :: forall route pathParams queryParams reqHeaders body respVariant
       successRow errorRow errorRL
       handlersRow handlersRL handled
   . RouteHandler route pathParams queryParams reqHeaders body respVariant
  -- Split response into success/error
  => SplitResponse respVariant successRow errorRow
  -- Expand success sub-variant into full variant
  => Row.Union successRow errorRow respVariant
  -- Build error handlers record
  => RL.RowToList errorRow errorRL
  => BuildErrorHandlers errorRL respVariant handlersRow
  -- onMatch constraints for the error variant handler
  => RL.RowToList handlersRow handlersRL
  => VariantMatchCases handlersRL handled
       ( Om
           { path :: Record pathParams
           , query :: Record queryParams
           , headers :: Record reqHeaders
           , body :: body
           }
           ()
           (Variant respVariant)
       )
  => Row.Union handled (exception :: Error) (exception :: Error | errorRow)
  -- respondNow support
  => Row.Lacks "_respondNow" errorRow
  => Om
       { path :: Record pathParams
       , query :: Record queryParams
       , headers :: Record reqHeaders
       , body :: body
       }
       (_respondNow :: Variant successRow | errorRow)
       (Variant successRow)
  -> Handler route
handle om = mkHandler \ctx ->
  runOm ctx { exception: Aff.throwError } $
    handleErrors' errorHandler (Variant.expand <$> om')
  where
  om' = handleErrors' respondNowHandler om
  respondNowHandler = Variant.on (Proxy :: Proxy "_respondNow") pure throwError
  errorHandler errVariant =
    Variant.onMatch
      ( Builder.buildFromScratch
          (buildErrorHandlers (Proxy :: Proxy errorRL) (Proxy :: Proxy respVariant))
      )
      throwError
      errVariant
