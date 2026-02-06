module Test.ParserTest where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Int as Int
import Data.Array as Array
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Type.RowList (class RowToList, RowList, Cons, Nil)
import Record as Record
import Record.Builder as Builder
import Effect (Effect)
import Effect.Console (log, logShow)

-- Import path types from production module
import Yoga.HTTP.API.Path (Path, PathCons, Param, QueryParams, Required, type (/), type (:), type (:?))

-- Let's start with a simple example
-- Given: Path ("users" / "id" : Int / "posts")
-- Parse: "/users/124/posts"
-- Result: { id :: Int }

-- First, let's try a really simple case: just one capture
type SimplePath = Path ("users" / "id" : Int)

-- We need a typeclass that can parse a path string and extract the captures
class ParsePath :: forall k. k -> Type -> Constraint
class ParsePath path result | path -> result where
  parsePath :: Proxy path -> String -> Maybe result

-- Start with the simplest possible case: a path with one segment and one capture
-- Path ("users" / "id" : Int)
-- We need to parse this step by step

-- First, let's define what the result type should look like for different path structures
class PathResult :: forall k. k -> Type -> Constraint
class PathResult path result | path -> result

-- For a Param, the result is just the value
instance pathResultParam :: PathResult (Param name ty) ty

-- For a simple string segment followed by a param, we need to extract just the param
-- This is getting complex... let's think differently

-- Let's use a simpler approach: define what segments we expect and parse them
class ParseSegment :: forall k. k -> Constraint
class ParseSegment segment where
  -- Returns Nothing if this segment doesn't match, Just remaining if it does
  matchSegment :: Proxy segment -> String -> Maybe String

-- A literal string segment must match exactly
instance parseSegmentString :: IsSymbol s => ParseSegment s where
  matchSegment _ path =
    let
      segment = reflectSymbol (Proxy :: Proxy s)
      prefix = "/" <> segment
    in
      if String.take (String.length prefix) path == prefix then Just (String.drop (String.length prefix) path)
      else Nothing

-- Now let's add parsing for captures
-- A capture should extract the next segment and try to parse it as the given type
class ParseCapture :: Type -> Constraint
class ParseCapture ty where
  -- Parse a capture from the path, returns (parsed value, remaining path)
  parseCapture :: String -> Maybe { value :: ty, remaining :: String }

-- For Int, parse until the next / or end of string
instance parseCaptureInt :: ParseCapture Int where
  parseCapture path =
    let
      -- Skip leading /
      path' = if String.take 1 path == "/" then String.drop 1 path else path
      -- Take until next / or end
      segment = case String.indexOf (String.Pattern "/") path' of
        Just idx -> String.take idx path'
        Nothing -> path'
      remaining = String.drop (String.length segment) path'
    in
      case Int.fromString segment of
        Just value -> Just { value, remaining }
        Nothing -> Nothing

-- For String, just take the next segment
instance parseCaptureString :: ParseCapture String where
  parseCapture path =
    let
      path' = if String.take 1 path == "/" then String.drop 1 path else path
      segment = case String.indexOf (String.Pattern "/") path' of
        Just idx -> String.take idx path'
        Nothing -> path'
      remaining = String.drop (String.length segment) path'
    in
      Just { value: segment, remaining }

-- Test the simple case first
test1 :: Maybe String
test1 = matchSegment (Proxy :: Proxy "users") "/users/124"

-- Should return: Just "/124"

test2 :: Maybe String
test2 = matchSegment (Proxy :: Proxy "posts") "/users/124"

-- Should return: Nothing

test3 :: Maybe { value :: Int, remaining :: String }
test3 = parseCapture "/124/posts"

-- Should return: Just { value: 124, remaining: "/posts" }

test4 :: Maybe { value :: String, remaining :: String }
test4 = parseCapture "/hello/world"

-- Should return: Just { value: "hello", remaining: "/world" }

-- Now let's parse a complete path into a record
-- For Path ("users" / "id" : Int / "posts"), we want to extract { id :: Int }

-- We need a typeclass that builds up a record from path segments
-- Use instance chains to avoid overlapping
class ParsePathSegments :: forall k. k -> Row Type -> Constraint
class ParsePathSegments segments result | segments -> result where
  parsePathSegments :: Proxy segments -> String -> Maybe { captures :: Record result, remaining :: String }

-- PathCons: parse left, then parse right, merge the records (most specific, try first)
instance parsePathSegmentsCons ::
  ( ParsePathSegments l rowL
  , ParsePathSegments r rowR
  , Row.Union rowL rowR result
  , Row.Nub result result -- Remove duplicates
  ) =>
  ParsePathSegments (PathCons l r) result where
  parsePathSegments _ path = do
    { captures: capturesL, remaining: remaining1 } <- parsePathSegments (Proxy :: Proxy l) path
    { captures: capturesR, remaining: remaining2 } <- parsePathSegments (Proxy :: Proxy r) remaining1
    pure { captures: Record.merge capturesL capturesR, remaining: remaining2 }

-- A Param captures a value and adds it to the record
else instance parsePathSegmentsParam ::
  ( IsSymbol name
  , ParseCapture ty
  , Row.Cons name ty () row
  , Row.Lacks name ()
  ) =>
  ParsePathSegments (Param name ty) row where
  parsePathSegments _ path =
    case parseCapture path of
      Just { value, remaining } ->
        let
          captures = Builder.build (Builder.insert (Proxy :: Proxy name) value) {}
        in
          Just { captures, remaining }
      Nothing -> Nothing

-- Base case: a literal string segment (no captures, fallback)
else instance parsePathSegmentsString :: IsSymbol s => ParsePathSegments s () where
  parsePathSegments _ path =
    case matchSegment (Proxy :: Proxy s) path of
      Just remaining -> Just { captures: {}, remaining }
      Nothing -> Nothing

-- Now test a real path!
type TestPath = "users" / "id" : Int / "posts"

test5 :: Maybe { captures :: { id :: Int }, remaining :: String }
test5 = parsePathSegments (Proxy :: Proxy TestPath) "/users/124/posts"

-- Should return: Just { captures: { id: 124 }, remaining: "" }

test6 :: Maybe { captures :: { id :: Int }, remaining :: String }
test6 = parsePathSegments (Proxy :: Proxy TestPath) "/users/hello/posts"

-- Should return: Nothing (can't parse "hello" as Int)

-- Now let's add query parameter parsing
-- Query params can be optional (Maybe values) or required (Required wrapper)
-- Required is imported from Yoga.Fastify.Om.Path

-- Helper to parse query string into key-value pairs
parseQueryString :: String -> Array { key :: String, value :: String }
parseQueryString queryString =
  let
    -- Remove leading "?" if present
    qs = if String.take 1 queryString == "?" then String.drop 1 queryString else queryString
    pairs = String.split (Pattern "&") qs
  in
    pairs # Array.mapMaybe \pair ->
      case String.indexOf (Pattern "=") pair of
        Just idx ->
          let
            key = String.take idx pair
            value = String.drop (idx + 1) pair
          in
            Just { key, value }
        Nothing -> Nothing

-- Parse a specific query param by name
parseQueryParam :: forall ty. ParseCapture ty => String -> String -> Maybe ty
parseQueryParam queryString paramName =
  let
    pairs = parseQueryString queryString
  in
    case Array.find (\p -> p.key == paramName) pairs of
      Just { value } -> map _.value (parseCapture ("/" <> value))
      Nothing -> Nothing

-- Build a record of query params (Required becomes plain type, others become Maybe)
-- Returns Either with missing required params as errors
class ParseQueryParams :: Row Type -> Row Type -> Constraint
class ParseQueryParams params result | params -> result where
  parseQueryParams :: Proxy params -> String -> Either (Array String) (Record result)

-- We need to handle Required vs optional params differently
-- This requires RowToList to iterate over the params
class ParseQueryParamsList :: RowList Type -> Row Type -> Constraint
class ParseQueryParamsList list result | list -> result where
  parseQueryParamsList :: Proxy list -> String -> Either (Array String) (Record result)

instance parseQueryParamsListNil :: ParseQueryParamsList Nil () where
  parseQueryParamsList _ _ = Right {}

-- Required param: must be present or we error
instance parseQueryParamsListConsRequired ::
  ( IsSymbol name
  , ParseCapture ty
  , ParseQueryParamsList tail tailResult
  , Row.Cons name ty tailResult result
  , Row.Lacks name tailResult
  ) =>
  ParseQueryParamsList (Cons name (Required ty) tail) result where
  parseQueryParamsList _ queryString =
    let
      paramName = reflectSymbol (Proxy :: Proxy name)
      value = parseQueryParam queryString paramName
      restResult = parseQueryParamsList (Proxy :: Proxy tail) queryString
    in
      case value, restResult of
        Just v, Right rest -> Right $ Builder.build (Builder.insert (Proxy :: Proxy name) v) rest
        Nothing, Right _ -> Left [ "Missing required query parameter: " <> paramName ]
        Just _, Left errs -> Left errs
        Nothing, Left errs -> Left ([ "Missing required query parameter: " <> paramName ] <> errs)

-- Optional param: always succeeds with Maybe
else instance parseQueryParamsListConsOptional ::
  ( IsSymbol name
  , ParseCapture ty
  , ParseQueryParamsList tail tailResult
  , Row.Cons name (Maybe ty) tailResult result
  , Row.Lacks name tailResult
  ) =>
  ParseQueryParamsList (Cons name ty tail) result where
  parseQueryParamsList _ queryString =
    let
      paramName = reflectSymbol (Proxy :: Proxy name)
      value = parseQueryParam queryString paramName
      restResult = parseQueryParamsList (Proxy :: Proxy tail) queryString
    in
      case restResult of
        Right rest -> Right $ Builder.build (Builder.insert (Proxy :: Proxy name) value) rest
        Left errs -> Left errs

instance parseQueryParamsImpl ::
  ( RowToList params list
  , ParseQueryParamsList list result
  ) =>
  ParseQueryParams params result where
  parseQueryParams _ = parseQueryParamsList (Proxy :: Proxy list)

-- Now parse a complete path with query params
-- Returns Either with errors for invalid paths or missing required query params
class ParseFullPath :: forall k. k -> Row Type -> Row Type -> Row Type -> Constraint
class ParseFullPath path queryParams pathResult queryResult | path queryParams -> pathResult queryResult where
  parseFullPath :: Proxy path -> Proxy queryParams -> String -> Either (Array String) { path :: Record pathResult, query :: Record queryResult }

instance parseFullPathImpl ::
  ( ParsePathSegments segments pathResult
  , ParseQueryParams queryParams queryResult
  ) =>
  ParseFullPath (QueryParams (Path segments) (Record queryParams)) queryParams pathResult queryResult where
  parseFullPath _ _ input =
    let
      -- Split on ? to separate path and query
      parts = String.split (Pattern "?") input
      pathPart = Array.head parts # fromMaybe ""
      queryPart = Array.index parts 1 # fromMaybe ""
    in
      case parsePathSegments (Proxy :: Proxy segments) pathPart of
        Just { captures: pathCaptures, remaining: "" } ->
          case parseQueryParams (Proxy :: Proxy queryParams) queryPart of
            Right query -> Right { path: pathCaptures, query }
            Left errs -> Left errs
        _ -> Left [ "Invalid path: " <> pathPart ]

-- Test with optional query params
type TestPathWithQuery = Path ("users" / "id" : Int / "posts") :? { limit :: Int, offset :: Int }

test7 :: Either (Array String) { path :: { id :: Int }, query :: { limit :: Maybe Int, offset :: Maybe Int } }
test7 = parseFullPath (Proxy :: Proxy TestPathWithQuery) (Proxy :: Proxy (limit :: Int, offset :: Int)) "/users/124/posts?limit=10&offset=20"

-- Should return: Right { path: { id: 124 }, query: { limit: Just 10, offset: Just 20 } }

test8 :: Either (Array String) { path :: { id :: Int }, query :: { limit :: Maybe Int, offset :: Maybe Int } }
test8 = parseFullPath (Proxy :: Proxy TestPathWithQuery) (Proxy :: Proxy (limit :: Int, offset :: Int)) "/users/124/posts?limit=10"

-- Should return: Right { path: { id: 124 }, query: { limit: Just 10, offset: Nothing } }

test9 :: Either (Array String) { path :: { id :: Int }, query :: { limit :: Maybe Int, offset :: Maybe Int } }
test9 = parseFullPath (Proxy :: Proxy TestPathWithQuery) (Proxy :: Proxy (limit :: Int, offset :: Int)) "/users/124/posts"

-- Should return: Right { path: { id: 124 }, query: { limit: Nothing, offset: Nothing } }

-- Test with Required query params
type TestPathWithRequired = Path ("users" / "id" : Int / "posts") :? { limit :: Required Int, offset :: Int }

test10 :: Either (Array String) { path :: { id :: Int }, query :: { limit :: Int, offset :: Maybe Int } }
test10 = parseFullPath (Proxy :: Proxy TestPathWithRequired) (Proxy :: Proxy (limit :: Required Int, offset :: Int)) "/users/124/posts?limit=10&offset=20"

-- Should return: Right { path: { id: 124 }, query: { limit: 10, offset: Just 20 } }

test11 :: Either (Array String) { path :: { id :: Int }, query :: { limit :: Int, offset :: Maybe Int } }
test11 = parseFullPath (Proxy :: Proxy TestPathWithRequired) (Proxy :: Proxy (limit :: Required Int, offset :: Int)) "/users/124/posts?limit=10"

-- Should return: Right { path: { id: 124 }, query: { limit: 10, offset: Nothing } }

test12 :: Either (Array String) { path :: { id :: Int }, query :: { limit :: Int, offset :: Maybe Int } }
test12 = parseFullPath (Proxy :: Proxy TestPathWithRequired) (Proxy :: Proxy (limit :: Required Int, offset :: Int)) "/users/124/posts"

-- Should return: Left ["Missing required query parameter: limit"]

main :: Effect Unit
main = do
  log "Testing segment parsing:"
  logShow $ "matchSegment 'users' '/users/124': " <> show test1
  logShow $ "matchSegment 'posts' '/users/124': " <> show test2
  log "\nTesting capture parsing:"
  logShow $ "parseCapture Int '/124/posts': " <> show test3
  logShow $ "parseCapture String '/hello/world': " <> show test4
  log "\nTesting complete path parsing:"
  logShow $ "parse '/users/124/posts': " <> show test5
  logShow $ "parse '/users/hello/posts' (should fail): " <> show test6
  log "\nTesting path with optional query params:"
  logShow $ "parse '/users/124/posts?limit=10&offset=20': " <> show test7
  logShow $ "parse '/users/124/posts?limit=10': " <> show test8
  logShow $ "parse '/users/124/posts': " <> show test9
  log "\nTesting path with Required query params:"
  logShow $ "parse '/users/124/posts?limit=10&offset=20': " <> show test10
  logShow $ "parse '/users/124/posts?limit=10': " <> show test11
  logShow $ "parse '/users/124/posts' (should error): " <> show test12
