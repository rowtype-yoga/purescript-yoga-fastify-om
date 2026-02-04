# Idiomatic Om Integration for Fastify

## Current State (Not Idiomatic)

```purescript
import Yoga.Fastify.Om as FastifyOm

main = do
  app <- liftEffect $ Fastify.fastify {}
  
  -- Handler runs in Aff, not Om - can't access context!
  FastifyOm.get (RouteURL "/users") handler app
  where
  handler req reply = do
    -- In Aff, must manually extract everything
    hdrs <- liftEffect $ Fastify.headers req
    params <- liftEffect $ Fastify.params req
    bodyMaybe <- liftEffect $ Fastify.body req
    
    -- Can't use ask, can't access shared context
    -- Must pass dependencies manually
```

**Problems:**
- ❌ Handlers run in `Aff`, not `Om`
- ❌ Can't use `ask` to access context
- ❌ No dependency injection
- ❌ Must manually extract request data
- ❌ No composability with Om ecosystem

---

## Proposed API (Idiomatic Om)

### Option 1: Request Context Pattern (Explicit)

**Handler type gets request in context:**

```purescript
module Main where

import Prelude
import Yoga.Fastify.Om as F
import Yoga.Om (Om, ask, runOm)
import Effect.Aff (launchAff_)

-- Your app context
type AppContext =
  { db :: DatabaseConnection
  , logger :: Logger
  , config :: AppConfig
  }

-- Request context (injected automatically)
type RequestContext =
  { headers :: Object String
  , params :: Object String
  , query :: Object Foreign
  , body :: Maybe Foreign
  }

-- Your error type
data AppError = Unauthorized | NotFound | ServerError String

-- Handler type: Om with both app and request context
type Handler = Om (AppContext & RequestContext) AppError Unit

-- Usage example
getUserHandler :: FastifyReply -> Handler
getUserHandler reply = do
  -- Access both app context and request context!
  { db, logger, headers, params } <- ask
  
  -- Get auth from headers
  authToken <- case lookup "authorization" headers of
    Nothing -> throwError Unauthorized
    Just token -> pure token
  
  -- Get user ID from params
  userId <- case lookup "id" params of
    Nothing -> throwError NotFound
    Just id -> pure id
  
  -- Use app dependencies
  log logger $ "Fetching user: " <> userId
  user <- fetchUser db userId
  
  -- Send response
  F.sendJson (encodeUser user) reply

-- Register routes with Om handlers
main = launchAff_ do
  app <- liftEffect $ F.fastify {}
  
  -- Register Om-aware route
  -- The library injects request context automatically
  F.getOm (RouteURL "/users/:id") getUserHandler app
  
  -- Run with app context
  let appContext = { db, logger, config }
  F.listenOm { port: Port 3000, host: Host "0.0.0.0" } appContext app
```

**Benefits:**
- ✅ Full Om monad access
- ✅ `ask` works for both app and request context
- ✅ Clean dependency injection
- ✅ Automatic request parsing

---

### Option 2: Request as Parameter (Simpler)

**Handler explicitly receives request:**

```purescript
-- Handler receives request value, not in context
type Handler = FastifyRequest -> FastifyReply -> Om AppContext AppError Unit

getUserHandler :: FastifyRequest -> FastifyReply -> Om AppContext AppError Unit
getUserHandler req reply = do
  -- Access app context via ask
  { db, logger } <- ask
  
  -- Request operations lifted to Om
  headers <- F.headers req  -- Already in Om!
  params <- F.params req
  
  authToken <- case lookup "authorization" headers of
    Nothing -> throwError Unauthorized
    Just token -> pure token
  
  userId <- case lookup "id" params of
    Nothing -> throwError NotFound
    Just id -> pure id
  
  log logger $ "Fetching user: " <> userId
  user <- fetchUser db userId
  
  F.sendJson (encodeUser user) reply

main = launchAff_ do
  app <- liftEffect $ F.fastify {}
  
  -- Register Om handler - request passed as parameter
  F.getOm (RouteURL "/users/:id") getUserHandler app
  
  -- Run with app context
  F.listenOm { port: Port 3000, host: Host "0.0.0.0" } appContext app
```

**Benefits:**
- ✅ Om monad for app context
- ✅ Explicit request parameter
- ✅ Simpler (no automatic injection)
- ✅ More flexible

---

### Option 3: Middleware Pattern (Most Powerful)

**Compose middleware for common patterns:**

```purescript
import Yoga.Fastify.Om.Middleware as Middleware

-- Middleware to extract and validate auth
requireAuth :: Middleware AppContext AppError { userId :: String }
requireAuth = Middleware.fromHeaders \headers -> do
  token <- case lookup "authorization" headers of
    Nothing -> throwError Unauthorized
    Just t -> pure t
  
  -- Validate token, return user ID
  validateToken token

-- Middleware to parse JSON body
requireJson :: forall a. (Foreign -> Either String a) -> Middleware AppContext AppError { parsed :: a }
requireJson decode = Middleware.fromBody \bodyMaybe -> do
  body <- case bodyMaybe of
    Nothing -> throwError $ BadRequest "Missing body"
    Just b -> pure b
  
  case decode body of
    Left err -> throwError $ BadRequest err
    Right val -> pure { parsed: val }

-- Handler with middleware
createUserHandler :: FastifyReply -> Om (AppContext & { userId :: String, parsed :: CreateUserRequest }) AppError Unit
createUserHandler reply = do
  { db, userId, parsed } <- ask  -- Auth + parsed data in context!
  
  -- userId from auth middleware
  -- parsed from JSON middleware
  newUser <- createUser db userId parsed
  
  F.status (StatusCode 201) reply
  F.sendJson (encodeUser newUser) reply

main = launchAff_ do
  app <- liftEffect $ F.fastify {}
  
  -- Compose middleware + handler
  F.postOm (RouteURL "/users")
    ( requireAuth
    >>> requireJson decodeCreateUserRequest
    >>> createUserHandler
    )
    app
  
  F.listenOm { port: Port 3000, host: Host "0.0.0.0" } appContext app
```

**Benefits:**
- ✅ Composable middleware
- ✅ Reusable auth/validation logic
- ✅ Type-safe context extension
- ✅ Clean separation of concerns

---

## Comparison

| Pattern | Complexity | Flexibility | Type Safety | Learning Curve |
|---------|------------|-------------|-------------|----------------|
| **Option 1: Request in Context** | Medium | Medium | High | Medium |
| **Option 2: Request as Parameter** ⭐ | Low | High | High | Low |
| **Option 3: Middleware** | High | Very High | Very High | High |

---

## Recommended Approach

**Start with Option 2** (Request as Parameter):

### Core API

```purescript
-- Om-aware route handlers
type OmHandler ctx err = FastifyRequest -> FastifyReply -> Om ctx err Unit

-- Route registration
getOm :: forall ctx err. RouteURL -> OmHandler ctx err -> Fastify -> Om ctx err Unit
postOm :: forall ctx err. RouteURL -> OmHandler ctx err -> Fastify -> Om ctx err Unit
-- ... etc

-- Request operations (already lifted to Om)
headers :: forall ctx err. FastifyRequest -> Om ctx err (Object String)
params :: forall ctx err. FastifyRequest -> Om ctx err (Object String)
query :: forall ctx err. FastifyRequest -> Om ctx err (Object Foreign)
body :: forall ctx err. FastifyRequest -> Om ctx err (Maybe Foreign)

-- Reply operations (already lifted to Om)
status :: forall ctx err. StatusCode -> FastifyReply -> Om ctx err FastifyReply
sendJson :: forall ctx err. Foreign -> FastifyReply -> Om ctx err Unit

-- Server lifecycle with context
listenOm :: forall ctx err. ListenOptions -> ctx -> Fastify -> Om ctx err String
```

### Complete Example

```purescript
module Main where

import Prelude
import Yoga.Fastify.Om as F
import Yoga.Om (Om, ask, runOm, throwError)
import Effect.Aff (launchAff_)
import Foreign.Object as Object

-- App context
type AppContext =
  { db :: DatabaseConnection
  , logger :: Logger
  , jwtSecret :: String
  }

data AppError
  = Unauthorized String
  | NotFound String
  | BadRequest String
  | ServerError String

-- Helper: get required header
requireHeader :: forall ctx err. String -> Object String -> Om ctx err String
requireHeader name headers = case Object.lookup name headers of
  Nothing -> throwError $ Unauthorized $ "Missing header: " <> name
  Just val -> pure val

-- Helper: get required param
requireParam :: forall ctx err. String -> Object String -> Om ctx err String
requireParam name params = case Object.lookup name params of
  Nothing -> throwError $ NotFound $ "Missing param: " <> name
  Just val -> pure val

-- Route handlers
getUserHandler :: FastifyRequest -> FastifyReply -> Om AppContext AppError Unit
getUserHandler req reply = do
  -- Access app context
  { db, logger } <- ask
  
  -- Get request data (all in Om!)
  headers <- F.headers req
  params <- F.params req
  
  -- Validate auth
  authToken <- requireHeader "authorization" headers
  userId <- validateJwt authToken
  
  -- Get user ID from route params
  targetUserId <- requireParam "id" params
  
  -- Business logic
  log logger $ "User " <> userId <> " fetching user " <> targetUserId
  user <- fetchUser db targetUserId
  
  -- Send response
  F.sendJson (encodeUser user) reply

createUserHandler :: FastifyRequest -> FastifyReply -> Om AppContext AppError Unit
createUserHandler req reply = do
  { db, logger } <- ask
  
  headers <- F.headers req
  bodyMaybe <- F.body req
  
  -- Auth
  authToken <- requireHeader "authorization" headers
  userId <- validateJwt authToken
  
  -- Parse body
  body <- case bodyMaybe of
    Nothing -> throwError $ BadRequest "Missing body"
    Just b -> pure b
  
  createUserReq <- case decodeCreateUser body of
    Left err -> throwError $ BadRequest err
    Right val -> pure val
  
  -- Business logic
  log logger $ "User " <> userId <> " creating user"
  newUser <- createUser db createUserReq
  
  -- Send response
  _ <- F.status (StatusCode 201) reply
  F.sendJson (encodeUser newUser) reply

listUsersHandler :: FastifyRequest -> FastifyReply -> Om AppContext AppError Unit
listUsersHandler req reply = do
  { db, logger } <- ask
  
  headers <- F.headers req
  query <- F.query req
  
  -- Auth
  authToken <- requireHeader "authorization" headers
  userId <- validateJwt authToken
  
  -- Parse query params
  let limit = parseLimit query
  let offset = parseOffset query
  
  -- Business logic
  log logger $ "User " <> userId <> " listing users"
  users <- listUsers db { limit, offset }
  
  F.sendJson (encodeUserList users) reply

-- Error handler
handleError :: AppError -> FastifyReply -> Om AppContext AppError Unit
handleError err reply = do
  { logger } <- ask
  
  case err of
    Unauthorized msg -> do
      log logger $ "Unauthorized: " <> msg
      _ <- F.status (StatusCode 401) reply
      F.sendJson (encodeError "Unauthorized" msg) reply
    
    NotFound msg -> do
      log logger $ "Not found: " <> msg
      _ <- F.status (StatusCode 404) reply
      F.sendJson (encodeError "Not found" msg) reply
    
    BadRequest msg -> do
      log logger $ "Bad request: " <> msg
      _ <- F.status (StatusCode 400) reply
      F.sendJson (encodeError "Bad request" msg) reply
    
    ServerError msg -> do
      log logger $ "Server error: " <> msg
      _ <- F.status (StatusCode 500) reply
      F.sendJson (encodeError "Internal error" "Something went wrong") reply

-- Main server setup
main :: Effect Unit
main = launchAff_ do
  -- Create Fastify app
  app <- liftEffect $ F.fastify { logger: false }
  
  -- Build app context
  db <- connectDatabase
  logger <- createLogger
  let appContext = { db, logger, jwtSecret: "secret123" }
  
  -- Register routes (all run in Om with context!)
  runOm appContext unit do
    F.getOm (RouteURL "/users") listUsersHandler app
    F.getOm (RouteURL "/users/:id") getUserHandler app
    F.postOm (RouteURL "/users") createUserHandler app
    
    -- Start server
    address <- F.listenOm { port: Port 3000, host: Host "0.0.0.0" } app
    liftEffect $ Console.log $ "🚀 Server running at " <> address
```

---

## Implementation Plan

1. **New types:**
   - `type OmHandler ctx err = FastifyRequest -> FastifyReply -> Om ctx err Unit`

2. **Route registration:**
   - `getOm`, `postOm`, `putOm`, `deleteOm`, `patchOm`
   - Internally wrap handler to run in Om context

3. **Request operations:**
   - Already done! Just need to ensure they work in Om

4. **Error handling:**
   - Add `catchOmError` to handle Om errors
   - Convert to HTTP responses

5. **Examples:**
   - Full CRUD API example
   - Auth middleware example
   - Error handling example

---

## Questions for You

1. **Like Option 2 (Request as Parameter)?** 
   - Simple and flexible
   - Clear what's in context vs parameters

2. **Want middleware (Option 3) eventually?**
   - More complex but very powerful
   - Can add later

3. **Error handling:**
   - Should we auto-catch Om errors and convert to 500?
   - Or explicit error handler registration?

4. **Context injection:**
   - How to provide app context to routes?
   - `runOm context unit` wrapper?
   - Or pass to each route?

Let me know what you think! 🚀
