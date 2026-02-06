module Yoga.Fastify.Om.Route.OmHandler
  ( handle
  , respond
  , respondWithHeaders
  , reject
  , rejectWithHeaders
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
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Om.Route.Response (Response(..))
import Yoga.Fastify.Om.Route.RouteHandler (Handler, class RouteHandler, mkHandler)
import Yoga.Om (Om, handleErrors', runOm)

--------------------------------------------------------------------------------
-- Is2xxStatus: Map status labels to type-level Boolean
--------------------------------------------------------------------------------

-- | Determine whether a variant label corresponds to a 2xx HTTP status code.
class Is2xxStatus (label :: Symbol) (is2xx :: Boolean) | label -> is2xx

instance Is2xxStatus "ok" True
else instance Is2xxStatus "created" True
else instance Is2xxStatus "accepted" True
else instance Is2xxStatus "noContent" True
else instance Is2xxStatus label False

--------------------------------------------------------------------------------
-- SplitResponse: Split response row into success (2xx) / error (non-2xx)
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- BuildErrorHandlers: Build onMatch record from error RowList
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- respond / reject: Ergonomic response helpers for Om handlers
--------------------------------------------------------------------------------

-- | Return a 2xx response in an Om handler with no headers.
-- |
-- | Example:
-- | ```purescript
-- | respond { ok: { id: 1, name: "Alice" } }
-- | ```
respond
  :: forall rec label body r1 r2 ctx err
   . RL.RowToList rec (RL.Cons label body RL.Nil)
  => Row.Cons label body () rec
  => IsSymbol label
  => Row.Cons label (Response () body) r1 r2
  => Record rec
  -> Om ctx err (Variant r2)

respond rec =
  let
    body = Record.get (Proxy :: Proxy label) rec
  in
    pure (Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body }))

-- | Return a 2xx response in an Om handler with custom headers.
-- |
-- | Example:
-- | ```purescript
-- | respondWithHeaders { created: { headers: { "Location": "/users/123" }, body: user } }
-- | ```
respondWithHeaders
  :: forall rec label headers body r1 r2 ctx err
   . RL.RowToList rec (RL.Cons label { headers :: Record headers, body :: body } RL.Nil)
  => Row.Cons label { headers :: Record headers, body :: body } () rec
  => IsSymbol label
  => Row.Cons label (Response headers body) r1 r2
  => Record rec
  -> Om ctx err (Variant r2)

respondWithHeaders rec =
  let
    { headers, body } = Record.get (Proxy :: Proxy label) rec
  in
    pure (Variant.inj (Proxy :: Proxy label) (Response { headers, body }))

-- | Throw a non-2xx response in an Om handler (short-circuits) with no headers.
-- |
-- | Example:
-- | ```purescript
-- | reject { notFound: { error: "User not found" } }
-- | ```
reject
  :: forall rec label body _r1 err _r2 ctx a
   . RL.RowToList rec (RL.Cons label body RL.Nil)
  => Row.Cons label body () rec
  => IsSymbol label
  => Row.Cons label (Response () body) _r1 err
  => Row.Cons label (Response () body) _r2 (exception :: Error | err)
  => Record rec
  -> Om ctx err a
reject rec =
  let
    body = Record.get (Proxy :: Proxy label) rec
  in
    throwError (Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body }))

-- | Throw a non-2xx response in an Om handler (short-circuits) with custom headers.
-- |
-- | Example:
-- | ```purescript
-- | rejectWithHeaders { unauthorized: { headers: { "WWW-Authenticate": "Bearer" }, body: { error: "Invalid token" } } }
-- | ```
rejectWithHeaders
  :: forall rec label headers body _r1 err _r2 ctx a
   . RL.RowToList rec (RL.Cons label { headers :: Record headers, body :: body } RL.Nil)
  => Row.Cons label { headers :: Record headers, body :: body } () rec
  => IsSymbol label
  => Row.Cons label (Response headers body) _r1 err
  => Row.Cons label (Response headers body) _r2 (exception :: Error | err)
  => Record rec
  -> Om ctx err a
rejectWithHeaders rec =
  let
    { headers, body } = Record.get (Proxy :: Proxy label) rec
  in
    throwError (Variant.inj (Proxy :: Proxy label) (Response { headers, body }))

--------------------------------------------------------------------------------
-- handle: Om-based route handler
--------------------------------------------------------------------------------

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
  => Om
       { path :: Record pathParams
       , query :: Record queryParams
       , headers :: Record reqHeaders
       , body :: body
       }
       errorRow
       (Variant successRow)
  -> Handler route
handle om = mkHandler \ctx ->
  runOm ctx { exception: Aff.throwError } $
    handleErrors' errorHandler (Variant.expand <$> om)
  where
  errorHandler errVariant =
    Variant.onMatch
      ( Builder.buildFromScratch
          (buildErrorHandlers (Proxy :: Proxy errorRL) (Proxy :: Proxy respVariant))
      )
      throwError
      errVariant
