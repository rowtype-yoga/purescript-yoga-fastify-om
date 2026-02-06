module Yoga.Fastify.Om.Route.Response
  ( Response(..)
  , ResponseData
  , class ToResponse
  , class ToResponseRL
  , respond
  , respondWith
  , respondNoHeaders
  , module Data.Variant
  ) where

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row (class Cons)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Response Data Type
--------------------------------------------------------------------------------

-- | Response data combining headers and body
-- | This is a data type (not type alias) to work with type class instances
data Response headers body = Response
  { headers :: Record headers
  , body :: body
  }

-- | Deprecated alias for backwards compatibility
type ResponseData headers body = Response headers body

--------------------------------------------------------------------------------
-- ToResponse: Convert record syntax to Response type
--------------------------------------------------------------------------------

-- | Convert user-friendly record syntax to Response type.
-- |
-- | Supports:
-- |   Response headers body → Response headers body (identity)
-- |   { body :: User } → Response () User
-- |   { headers :: { "Location" :: String }, body :: User } → Response ("Location" :: String) User
class ToResponse (recordType :: Type) (headers :: Row Type) (body :: Type) | recordType -> headers body

-- Identity instance: Response is already Response
instance toResponseIdentity :: ToResponse (Response headers body) headers body

-- Record instance: delegate to RowList-based helper
else instance toResponseRecord ::
  ( RL.RowToList recordRow rl
  , ToResponseRL rl headers body
  ) =>
  ToResponse (Record recordRow) headers body

-- | RowList-based helper to distinguish records with/without headers.
-- | Instance heads are distinguishable by the RowList structure.
class ToResponseRL (rl :: RL.RowList Type) (headers :: Row Type) (body :: Type) | rl -> headers body

-- { body :: b, headers :: Record h } (headers comes before body in sorted RowList)
instance toResponseRLBodyHeaders ::
  ToResponseRL (RL.Cons "body" body (RL.Cons "headers" (Record headers) RL.Nil)) headers body

-- { body :: b } (no headers)
else instance toResponseRLBodyOnly ::
  ToResponseRL (RL.Cons "body" body RL.Nil) () body

--------------------------------------------------------------------------------
-- Response Construction Helpers
--------------------------------------------------------------------------------

-- | Construct a variant response with full control over headers and body
-- |
-- | Example:
-- |   respond (Proxy :: _ "created")
-- |     (Response { headers: { "Location": "/users/123" }, body: user })
respond
  :: forall label headers body r1 r2
   . IsSymbol label
  => Cons label (Response headers body) r1 r2
  => Proxy label
  -> Response headers body
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
  => Cons label (Response headers body) r1 r2
  => Proxy label
  -> Record headers
  -> body
  -> Variant r2
respondWith label headers body =
  Variant.inj label (Response { headers, body })

-- | Construct a variant response with no custom headers (most common case)
-- |
-- | Example:
-- |   respondNoHeaders (Proxy :: _ "ok") user
-- |   respondNoHeaders (Proxy :: _ "notFound") { error: "Not found" }
respondNoHeaders
  :: forall @label body r1 r2
   . IsSymbol label
  => Cons label (Response () body) r1 r2
  => body
  -> Variant r2
respondNoHeaders body =
  Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body })
