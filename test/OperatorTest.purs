module Test.OperatorTest where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.RowList (class RowToList, RowList, Cons, Nil)
import Data.String as String

-- Import path types from production module
import Yoga.HTTP.API.Path (Path, PathCons, Param, QueryParams, type (/), type (:), type (:?))

-- Test usage
type TestPath1 = "users" : Int

type TestPath3 = Path ("users" / "id" : Int / "posts")

-- Or simpler: just use the operator for params
type TestPath4 = Path ("users" / "id" : Int / "posts")

-- Test usage examples with record-based query params
type TestPath5 = Path ("users" / "id" : Int) :? { limit :: Int }

-- Use a multi-segment path for this example
type TestPath6 = Path ("api" / "posts") :? { page :: Int, sort :: String }

-- More realistic example
type TestPath7 = Path ("api" / "users" / "id" : Int / "posts") :? { limit :: Int, offset :: Int, sort :: Boolean }

-- Rendering paths to OpenAPI format
-- We need type classes to convert type-level structures to runtime strings

-- Class to render path segments
-- Use instance chains to avoid overlapping instances
class RenderPathSegment :: forall k. k -> Constraint
class RenderPathSegment path where
  renderPathSegment :: Proxy path -> String

-- PathCons: render left and right, concatenate (most specific, try first)
instance renderPathSegmentCons :: (RenderPathSegment l, RenderPathSegment r) => RenderPathSegment (PathCons l r) where
  renderPathSegment _ = renderPathSegment (Proxy :: Proxy l) <> renderPathSegment (Proxy :: Proxy r)

-- Param: render as {name}
else instance renderPathSegmentParam :: IsSymbol name => RenderPathSegment (Param name ty) where
  renderPathSegment _ = "/{" <> reflectSymbol (Proxy :: Proxy name) <> "}"

-- A single symbol becomes a path segment (fallback, try last)
else instance renderPathSegmentSymbol :: IsSymbol s => RenderPathSegment s where
  renderPathSegment _ = "/" <> reflectSymbol (Proxy :: Proxy s)

-- Top-level class for rendering complete paths
class RenderPath :: forall k. k -> Constraint
class RenderPath path where
  renderPath :: Proxy path -> String

-- Path wrapper - this is the main entry point
instance renderPathPath :: RenderPathSegment segments => RenderPath (Path segments) where
  renderPath _ = renderPathSegment (Proxy :: Proxy segments)

-- Class to render query params
class RenderQueryParams :: Row Type -> Constraint
class RenderQueryParams params where
  renderQueryParams :: Proxy params -> Array String

-- We'll need to use RowToList for this

class RenderQueryParamsList :: RowList Type -> Constraint
class RenderQueryParamsList list where
  renderQueryParamsList :: Proxy list -> Array String

instance renderQueryParamsListNil :: RenderQueryParamsList Nil where
  renderQueryParamsList _ = []

instance renderQueryParamsListCons ::
  ( IsSymbol name
  , RenderQueryParamsList tail
  ) =>
  RenderQueryParamsList (Cons name ty tail) where
  renderQueryParamsList _ =
    [ reflectSymbol (Proxy :: Proxy name) ] <> renderQueryParamsList (Proxy :: Proxy tail)

instance renderQueryParamsImpl ::
  ( RowToList params list
  , RenderQueryParamsList list
  ) =>
  RenderQueryParams params where
  renderQueryParams _ = renderQueryParamsList (Proxy :: Proxy list)

-- Final rendering with query params
class RenderPathWithQuery :: forall k. k -> Row Type -> Constraint
class RenderPathWithQuery path params where
  renderPathWithQuery :: Proxy path -> Proxy params -> String

instance renderPathWithQueryInstance ::
  ( RenderPath path
  , RenderQueryParams params
  ) =>
  RenderPathWithQuery path params where
  renderPathWithQuery pathProxy paramsProxy =
    let
      path = renderPath pathProxy
      queryParams = renderQueryParams paramsProxy
      queryString = case queryParams of
        [] -> ""
        params -> "?" <> String.joinWith "&" (map (\p -> p <> "={" <> p <> "}") params)
    in
      path <> queryString

-- For paths without query params
renderSimplePath :: forall path. RenderPath path => Proxy path -> String
renderSimplePath = renderPath

-- Helper to render QueryParams type (Record row → unwrap to row)
renderFullPath
  :: forall path params
   . RenderPathWithQuery path params
  => Proxy (QueryParams path (Record params))
  -> String
renderFullPath _ = renderPathWithQuery (Proxy :: Proxy path) (Proxy :: Proxy params)

-- Test it!
test1 :: String
test1 = renderSimplePath (Proxy :: Proxy TestPath4)

-- Should produce: "/users/id/{id}/posts"

test2 :: String
test2 = renderFullPath (Proxy :: Proxy (Path ("api" / "posts") :? { page :: Int, sort :: String }))

-- Should produce: "/api/posts?page={page}&sort={sort}"

-- Main function to test the rendering
main :: Effect Unit
main = do
  log "Testing path rendering:"
  log $ "test1 (path with capture): " <> test1
  log $ "test2 (path with query params): " <> test2
  log $ "TestPath5: " <> renderFullPath (Proxy :: Proxy TestPath5)
  log $ "TestPath7: " <> renderFullPath (Proxy :: Proxy TestPath7)
