module Yoga.HTTP.API.Path
  ( Path
  , Root
  , Lit
  , Capture
  , PathCons
  , type (/)
  , Param
  , type (:)
  , QueryParams
  , type (:?)
  , Required
  , class PathPattern
  , pathPattern
  , class PathPatternSegs
  , pathPatternSegs
  -- , class PathParams  -- TODO: Export when instances are implemented
  , class ParseParam
  , parseParam
  , class ParsePath
  , parsePath
  ) where

import Prelude

import Data.Array (intercalate, uncons)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
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
-- |   Path Root                               -- /
-- |   Path (Lit "users")                      -- /users
-- |   Path (Lit "users" / Capture "id" Int)  -- /users/:id
-- |   Path (Lit "users" / Capture "id" Int / Lit "posts")  -- /users/:id/posts
data Path :: forall k. k -> Type
data Path segments

-- | Root path representing "/"
-- |
-- | Example:
-- |   Path Root  -- /
data Root

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

data PathCons :: forall k1 k2. k1 -> k2 -> Type
data PathCons left right

-- | Sugar for Capture — use with bare Symbols in the path DSL
-- |
-- | Example:
-- |   Path ("users" / "id" : Int / "posts")
-- |   -- equivalent to: Path (Lit "users" / Capture "id" Int / Lit "posts")
data Param (name :: Symbol) (ty :: Type)

infixr 8 type Param as :

-- | Attach query parameters to a path
-- |
-- | Example:
-- |   Path ("users" / "id" : Int) :? { limit :: Int, offset :: Required Int }
data QueryParams :: forall k. k -> Type -> Type
data QueryParams path params

infixl 1 type QueryParams as :?

-- | Marker for required query parameters (default is optional → Maybe)
-- |
-- | Example:
-- |   Path ("users") :? (limit :: Int, offset :: Required Int)
-- |   -- limit parses as Maybe Int, offset parses as Int (fails if missing)
data Required :: Type -> Type
data Required a

--------------------------------------------------------------------------------
-- PathPattern: Generate URL patterns for Fastify
--------------------------------------------------------------------------------

-- | Generate a Fastify-compatible URL pattern from a path type
-- |
-- | Examples:
-- |   pathPattern (Proxy :: _ (Path Root)) = "/"
-- |   pathPattern (Proxy :: _ (Path (Lit "users"))) = "/users"
-- |   pathPattern (Proxy :: _ (Path (Lit "users" / Capture "id" Int))) = "/users/:id"
-- |   pathPattern (Proxy :: _ ("users" / "id" : Int)) = "/users/:id"
class PathPattern :: forall k. k -> Constraint
class PathPattern path where
  pathPattern :: Proxy path -> String

-- QueryParams wrapper delegates to inner path
instance pathPatternQueryParams ::
  ( PathPattern path
  ) =>
  PathPattern (QueryParams path params) where
  pathPattern _ = pathPattern (Proxy :: Proxy path)

-- Path delegates to PathPatternSegs for the inner segments
else instance pathPatternPath ::
  PathPatternSegs segs =>
  PathPattern (Path segs) where
  pathPattern _ = pathPatternSegs (Proxy :: Proxy segs)

-- Bare segments (no Path wrapper) delegate to PathPatternSegs
else instance pathPatternBare ::
  PathPatternSegs segs =>
  PathPattern segs where
  pathPattern _ = pathPatternSegs (Proxy :: Proxy segs)

-- | Internal poly-kinded class for generating URL patterns from path segments.
class PathPatternSegs :: forall k. k -> Constraint
class PathPatternSegs segs where
  pathPatternSegs :: Proxy segs -> String

-- Root
instance pathPatternSegsRoot :: PathPatternSegs Root where
  pathPatternSegs _ = "/"

-- "/" symbol (alias for Root)
else instance pathPatternSegsSlash :: PathPatternSegs "/" where
  pathPatternSegs _ = "/"

-- Lit wrapper
else instance pathPatternSegsLit :: IsSymbol s => PathPatternSegs (Lit s) where
  pathPatternSegs _ = "/" <> reflectSymbol (Proxy :: Proxy s)

-- Capture
else instance pathPatternSegsCapture :: IsSymbol name => PathPatternSegs (Capture name ty) where
  pathPatternSegs _ = "/:" <> reflectSymbol (Proxy :: Proxy name)

-- Param sugar
else instance pathPatternSegsParam :: IsSymbol name => PathPatternSegs (Param name ty) where
  pathPatternSegs _ = "/:" <> reflectSymbol (Proxy :: Proxy name)

-- PathCons with Lit on left
else instance pathPatternSegsLitCons ::
  ( IsSymbol s
  , PathPatternSegs rest
  ) =>
  PathPatternSegs (PathCons (Lit s) rest) where
  pathPatternSegs _ = "/" <> reflectSymbol (Proxy :: Proxy s) <> pathPatternSegs (Proxy :: Proxy rest)

-- PathCons with Capture on left
else instance pathPatternSegsCaptureCons ::
  ( IsSymbol name
  , PathPatternSegs rest
  ) =>
  PathPatternSegs (PathCons (Capture name ty) rest) where
  pathPatternSegs _ = "/:" <> reflectSymbol (Proxy :: Proxy name) <> pathPatternSegs (Proxy :: Proxy rest)

-- PathCons with Param on left
else instance pathPatternSegsParamCons ::
  ( IsSymbol name
  , PathPatternSegs rest
  ) =>
  PathPatternSegs (PathCons (Param name ty) rest) where
  pathPatternSegs _ = "/:" <> reflectSymbol (Proxy :: Proxy name) <> pathPatternSegs (Proxy :: Proxy rest)

-- PathCons with bare Symbol on left (from : sugar)
else instance pathPatternSegsSymbolCons ::
  ( IsSymbol s
  , PathPatternSegs rest
  ) =>
  PathPatternSegs (PathCons s rest) where
  pathPatternSegs _ = "/" <> reflectSymbol (Proxy :: Proxy s) <> pathPatternSegs (Proxy :: Proxy rest)

-- Path wrapper (unwrap and delegate)
else instance pathPatternSegsPath :: PathPatternSegs segs => PathPatternSegs (Path segs) where
  pathPatternSegs _ = pathPatternSegs (Proxy :: Proxy segs)

-- Bare Symbol (single segment, from : sugar)
else instance pathPatternSegsSymbol :: IsSymbol s => PathPatternSegs s where
  pathPatternSegs _ = "/" <> reflectSymbol (Proxy :: Proxy s)

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
  parseParam :: String -> Either String ty

instance parseParamString :: ParseParam String where
  parseParam = Right

instance parseParamInt :: ParseParam Int where
  parseParam s = case Int.fromString s of
    Nothing -> Left $ "Expected an integer but got: " <> s
    Just n -> Right n

instance parseParamNumber :: ParseParam Number where
  parseParam s = case Int.fromString s of
    Nothing -> Left $ "Expected a number but got: " <> s
    Just n -> Right (Int.toNumber n) -- Simple version, doesn't handle floats


-- | Parse a URL string into a record of typed path parameters
-- |
-- | Examples:
-- |   parsePath @(Path Root) "/" = Just {}
-- |   parsePath @(Path (Lit "users")) "/users" = Just {}
-- |   parsePath @(Path (Lit "users" / Capture "id" Int)) "/users/123" = Just { id: 123 }
class ParsePath (path :: Type) (params :: Row Type) | path -> params where
  parsePath :: Proxy path -> String -> Maybe (Record params)

-- Base case: Root path - just verify it's "/"
instance parsePathRoot :: ParsePath (Path Root) () where
  parsePath _ url = if url == "/" then Just {} else Nothing

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
      [ segment ] -> case (parseParam segment :: Either String ty) of
        Left _ -> Nothing
        Right value -> pure $ unsafeCoerce { value } -- Simplified: we'd need proper record construction
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
        value <- case parseParam head of
          Left _ -> Nothing
          Right v -> Just v
        -- Parse the rest
        restRecord <- parsePath (Proxy :: _ (Path rest)) ("/" <> intercalate "/" tail)
        -- Merge: add the captured value to the rest record
        pure $ Record.insert (Proxy :: _ name) value restRecord
      Nothing -> Nothing
