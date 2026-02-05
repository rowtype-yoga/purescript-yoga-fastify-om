# Type-Safe Routing System

This project implements a complete type-safe routing system with bidirectional path transformation.

## Features

### 1. Type-Level Path Definition
Define routes using intuitive operators:
```purescript
type UserPostsRoute = Path ("api" / "users" / "id" :> Int / "posts") :? (limit :: Int, offset :: Int)
```

**Operators:**
- `:>` - Path capture (e.g., `"id" :> Int`)
- `/` - Path segment separator
- `:?` - Query parameters (as a record)

### 2. Rendering to OpenAPI Format
Convert type-level paths to OpenAPI-compatible strings:
```purescript
renderFullPath (Proxy :: Proxy UserPostsRoute)
-- Result: "/api/users/{id}/posts?limit={limit}&offset={offset}"
```

### 3. Parsing URLs to Typed Records
Parse actual URLs into strongly-typed records:
```purescript
parseFullPath (Proxy :: Proxy UserPostsRoute) "/api/users/123/posts?limit=10&offset=20"
-- Result: Right { path: { id: 123 }, query: { limit: Just 10, offset: Just 20 } }
```

### 4. Required vs Optional Query Parameters

**Optional (default):**
```purescript
type Route = Path (...) :? (limit :: Int, offset :: Int)
-- Parsed as: { limit :: Maybe Int, offset :: Maybe Int }
```

**Required:**
```purescript  
type Route = Path (...) :? (limit :: Required Int, offset :: Int)
-- Parsed as: { limit :: Int, offset :: Maybe Int }
```

Missing required parameters return descriptive errors:
```purescript
parseFullPath ... "/api/users/123/posts"
-- Result: Left ["Missing required query parameter: limit"]
```

### 5. Comprehensive Test Suite
- vitest-based tests for all functionality
- Tests for rendering, parsing, error handling
- Validates both success and error cases

## Example Usage

```purescript
-- Define a route
type UserPostsRoute = 
  Path ("api" / "users" / "userId" :> Int / "posts") 
    :? (limit :: Required Int, offset :: Int, sort :: String)

-- Render to OpenAPI
renderFullPath (Proxy :: Proxy UserPostsRoute)
-- "/api/users/{userId}/posts?limit={limit}&offset={offset}&sort={sort}"

-- Parse an actual request
parseFullPath (Proxy :: Proxy UserPostsRoute) 
  (Proxy :: Proxy (limit :: Required Int, offset :: Int, sort :: String))
  "/api/users/42/posts?limit=10&sort=desc"
-- Right { 
--   path: { userId: 42 }, 
--   query: { limit: 10, offset: Nothing, sort: Just "desc" } 
-- }
```

## Files

- `test/OperatorTest.purs` - Core path operators and rendering
- `test/ParserTest.purs` - URL parsing implementation  
- `test/OperatorTest.Spec.purs` - Rendering tests
- `test/ParserTest.Spec.purs` - Parser tests
- `test/Test.Operators.purs` - Main test suite

## Running Tests

```bash
bun install  # first time only
spago build
bun test
```

**All 20 tests passing!** ✅
