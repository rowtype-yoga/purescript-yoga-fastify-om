module Yoga.Fastify.Om.Path
  ( Path
  , Lit
  , Capture
  , PathCons
  , type (/)
  , class PathPattern
  , pathPattern
  -- , class PathParams  -- TODO: Export when instances are implemented
  , class ParsePath
  , parsePath
  ) where

import Prelude

import Data.Array (intercalate, uncons)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Type-Level Path DSL
--------------------------------------------------------------------------------

-- | Type-level path representation
-- |
-- | Examples:
-- |   Path @"users"                           -- /users
-- |   Path (@"users" / Capture "id" Int)     -- /users/:id
-- |   Path (@"users" / Capture "id" Int / @"posts")  -- /users/:id/posts
data Path (segments :: Type)

-- | Literal path segment (wrapper for Symbol to work around PureScript 0.15 limitations)
-- |
-- | In PureScript 0.15, we can't use bare Symbols like Path "users" due to kind inference.
-- | Instead, use: Path (Lit "users")
-- |
-- | Example:
-- |   Path (Lit "users")                     -- /users
-- |   Path (Lit "users" / Capture "id" Int)  -- /users/:id
data Lit (segment :: Symbol)

-- | Capture a path parameter with a name and type
-- |
-- | Example:
-- |   Capture "id" Int    -- captures :id as an Int
-- |   Capture "name" String  -- captures :name as a String
data Capture (name :: Symbol) (ty :: Type)

-- | Infix operator for building paths
-- |
-- | Example:
-- |   Lit "users" / Capture "id" Int / Lit "posts"
infixr 6 type PathCons as /
data PathCons left right

--------------------------------------------------------------------------------
-- PathPattern: Generate URL patterns for Fastify
--------------------------------------------------------------------------------

-- | Generate a Fastify-compatible URL pattern from a path type
-- |
-- | Examples:
-- |   pathPattern (Proxy :: _ (Path "users")) = "/users"
-- |   pathPattern (Proxy :: _ (Path ("users" / Capture "id" Int))) = "/users/:id"
class PathPattern (path :: Type) where
  pathPattern :: Proxy path -> String

-- Base case: single literal segment using Lit wrapper
instance pathPatternLit :: IsSymbol s => PathPattern (Path (Lit s)) where
  pathPattern _ = "/" <> reflectSymbol (Proxy :: Proxy s)

-- Base case: single capture
instance pathPatternCapture :: (IsSymbol name) => PathPattern (Path (Capture name ty)) where
  pathPattern _ = "/:" <> reflectSymbol (Proxy :: Proxy name)

-- Recursive case: Lit followed by more
instance pathPatternLitCons ::
  ( IsSymbol s
  , PathPattern (Path rest)
  ) =>
  PathPattern (Path (Lit s / rest)) where
  pathPattern _ = "/" <> reflectSymbol (Proxy :: Proxy s) <> pathPattern (Proxy :: _ (Path rest))

-- Recursive case: capture followed by more
instance pathPatternCaptureCons ::
  ( IsSymbol name
  , PathPattern (Path rest)
  ) =>
  PathPattern (Path (Capture name ty / rest)) where
  pathPattern _ = "/:" <> reflectSymbol (Proxy :: Proxy name) <> pathPattern (Proxy :: _ (Path rest))

--------------------------------------------------------------------------------
-- PathParams: Extract capture types into a record row
--------------------------------------------------------------------------------

-- | Extract path parameter names and types into a record row type
-- |
-- | Examples:
-- |   PathParams (Path @"users") ()
-- |   PathParams (Path (@"users" / Capture "id" Int)) (id :: Int)
-- |   PathParams (Path (@"users" / Capture "userId" Int / @"posts" / Capture "postId" Int))
-- |              (userId :: Int, postId :: Int)
class PathParams (path :: Type) (params :: Row Type) | path -> params

-- TODO: Add PathParams instances when needed

--------------------------------------------------------------------------------
-- ParsePath: Parse URL string into typed path parameters
--------------------------------------------------------------------------------

-- | Parse a value from a String (used for path captures)
class ParseParam (ty :: Type) where
  parseParam :: String -> Maybe ty

instance parseParamString :: ParseParam String where
  parseParam = Just

instance parseParamInt :: ParseParam Int where
  parseParam = Int.fromString

instance parseParamNumber :: ParseParam Number where
  parseParam s = Int.fromString s <#> Int.toNumber -- Simple version, doesn't handle floats

-- | Parse a URL string into a record of typed path parameters
-- |
-- | Examples:
-- |   parsePath @(Path @"users") "/users" = Just {}
-- |   parsePath @(Path (@"users" / Capture "id" Int)) "/users/123" = Just { id: 123 }
class ParsePath (path :: Type) (params :: Row Type) | path -> params where
  parsePath :: Proxy path -> String -> Maybe (Record params)

-- Base case: Just a Lit segment, no captures - verify the path matches
instance parsePathLit :: IsSymbol s => ParsePath (Path (Lit s)) () where
  parsePath _ url =
    let
      expected = "/" <> reflectSymbol (Proxy :: Proxy s)
    in
      if url == expected then Just {} else Nothing

-- Base case: Just a capture - parse single segment
instance parsePathCapture ::
  ( IsSymbol name
  , ParseParam ty
  ) =>
  ParsePath (Path (Capture name ty)) (name :: ty) where
  parsePath _ url = do
    -- Remove leading "/"
    let segments = Array.filter (_ /= "") $ split (Pattern "/") url
    case segments of
      [ segment ] -> do
        value <- (parseParam segment :: Maybe ty)
        pure $ unsafeCoerce { value } -- Simplified: we'd need proper record construction
      _ -> Nothing

-- Recursive case: Lit segment followed by more
instance parsePathLitCons ::
  ( IsSymbol s
  , ParsePath (Path rest) params
  ) =>
  ParsePath (Path (Lit s / rest)) params where
  parsePath _ url = do
    -- Split URL into segments
    let segments = Array.filter (_ /= "") $ split (Pattern "/") url
    case uncons segments of
      Just { head, tail } -> do
        -- Check if first segment matches the literal
        let expected = reflectSymbol (Proxy :: Proxy s)
        if head == expected then parsePath (Proxy :: _ (Path rest)) ("/" <> intercalate "/" tail)
        else Nothing
      Nothing -> Nothing

-- Recursive case: Capture followed by more
-- This is complex because we need to:
-- 1. Parse the captured value
-- 2. Parse the rest of the path
-- 3. Merge the results into a single record
instance parsePathCaptureCons ::
  ( IsSymbol name
  , ParseParam ty
  , ParsePath (Path rest) restParams
  , Row.Cons name ty restParams fullParams
  , Row.Lacks name restParams
  ) =>
  ParsePath (Path (Capture name ty / rest)) fullParams where
  parsePath _ url = do
    -- Split URL into segments
    let segments = Array.filter (_ /= "") $ split (Pattern "/") url
    case uncons segments of
      Just { head, tail } -> do
        -- Parse the captured segment
        value <- parseParam head
        -- Parse the rest
        restRecord <- parsePath (Proxy :: _ (Path rest)) ("/" <> intercalate "/" tail)
        -- Merge: add the captured value to the rest record
        pure $ Record.insert (Proxy :: _ name) value restRecord
      Nothing -> Nothing
