module Yoga.Fastify.Om.Route.Response
  ( ResponseData(..)
  , respond
  , respondWith
  , respondNoHeaders
  , module Data.Variant
  ) where

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Response Data Type
--------------------------------------------------------------------------------

-- | Response data combining headers and body
-- | This is a data type (not type alias) to work with type class instances
data ResponseData headers body = ResponseData
  { headers :: Record headers
  , body :: body
  }

--------------------------------------------------------------------------------
-- Response Construction Helpers
--------------------------------------------------------------------------------

-- | Construct a variant response with full control over headers and body
-- |
-- | Example:
-- |   respond (Proxy :: _ "created")
-- |     (ResponseData { headers: { "Location": "/users/123" }, body: user })
respond
  :: forall label headers body r1 r2
   . IsSymbol label
  => Cons label (ResponseData headers body) r1 r2
  => Proxy label
  -> ResponseData headers body
  -> Variant r2
respond label rd = Variant.inj label rd

-- | Construct a variant response with separate headers and body arguments
-- |
-- | Example:
-- |   respondWith (Proxy :: _ "created")
-- |     { "Location": "/users/123" }
-- |     user
respondWith
  :: forall label headers body r1 r2
   . IsSymbol label
  => Cons label (ResponseData headers body) r1 r2
  => Proxy label
  -> Record headers
  -> body
  -> Variant r2
respondWith label headers body =
  Variant.inj label (ResponseData { headers, body })

-- | Construct a variant response with no custom headers (most common case)
-- |
-- | Example:
-- |   respondNoHeaders (Proxy :: _ "ok") user
-- |   respondNoHeaders (Proxy :: _ "notFound") { error: "Not found" }
respondNoHeaders
  :: forall @label body r1 r2
   . IsSymbol label
  => Cons label (ResponseData () body) r1 r2
  => body
  -> Variant r2
respondNoHeaders body =
  Variant.inj (Proxy :: Proxy label) (ResponseData { headers: {}, body })
