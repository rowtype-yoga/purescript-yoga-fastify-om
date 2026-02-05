module Example.Server where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Fastify (Fastify, Host(..), Port(..))
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Om.Path (type (/), type (:?), Capture, Path)
import Yoga.Fastify.Om.Route (GET, POST, Route, Handler, NoBody, JSON, ResponseData, handleRoute, respondNoHeaders)

--------------------------------------------------------------------------------
-- Example Types
--------------------------------------------------------------------------------

type User = { id :: Int, name :: String, email :: String }
type CreateUserRequest = { name :: String, email :: String }
type ErrorResponse = { error :: String }

--------------------------------------------------------------------------------
-- Routes
--------------------------------------------------------------------------------

-- GET /health
type HealthRoute = Route GET
  (Path "health")
  { requestHeaders :: {}
  , requestBody :: NoBody
  }
  ( ok :: ResponseData () { status :: String }
  )

healthHandler :: Handler () () () Unit (ok :: ResponseData () { status :: String })
healthHandler _ = pure $ respondNoHeaders @"ok" { status: "healthy" }

-- GET /users/:id
type UserRoute = Route GET
  (Path ("users" / Capture "id" Int))
  { requestHeaders :: {}
  , requestBody :: NoBody
  }
  ( ok :: ResponseData () User
  , notFound :: ResponseData () ErrorResponse
  )

userHandler
  :: Handler (id :: Int) () () Unit
       ( ok :: ResponseData () User
       , notFound :: ResponseData () ErrorResponse
       )
userHandler { path } =
  if path.id == 1 then
    pure $ respondNoHeaders @"ok"
      { id: 1, name: "Alice", email: "alice@example.com" }
  else
    pure $ respondNoHeaders @"notFound"
      { error: "User not found" }

-- GET /users?limit=10
type UsersWithLimitRoute = Route GET
  (Path "users" :? (limit :: Int))
  { requestHeaders :: {}
  , requestBody :: NoBody
  }
  ( ok :: ResponseData () { users :: Array User, limit :: Maybe Int }
  )

usersWithLimitHandler
  :: Handler () (limit :: Maybe Int) () Unit
       ( ok :: ResponseData () { users :: Array User, limit :: Maybe Int }
       )
usersWithLimitHandler { query } = pure $ respondNoHeaders @"ok"
  { users:
      [ { id: 1, name: "Alice", email: "alice@example.com" }
      , { id: 2, name: "Bob", email: "bob@example.com" }
      ]
  , limit: query.limit
  }

-- POST /users
type CreateUserRoute = Route POST
  (Path "users")
  { requestHeaders :: {}
  , requestBody :: JSON CreateUserRequest
  }
  ( created :: ResponseData () User
  , badRequest :: ResponseData () ErrorResponse
  )

createUserHandler
  :: Handler () () () CreateUserRequest
       ( created :: ResponseData () User
       , badRequest :: ResponseData () ErrorResponse
       )
createUserHandler { requestBody } =
  if requestBody.name == "" then
    pure $ respondNoHeaders @"badRequest"
      { error: "Name cannot be empty" }
  else
    pure $ respondNoHeaders @"created"
      { id: 999, name: requestBody.name, email: requestBody.email }

--------------------------------------------------------------------------------
-- Server Setup
--------------------------------------------------------------------------------

createServer :: Effect Fastify
createServer = do
  fastify <- F.fastify {}

  -- Register routes
  handleRoute (Proxy :: _ HealthRoute) healthHandler fastify
  handleRoute (Proxy :: _ UserRoute) userHandler fastify
  handleRoute (Proxy :: _ UsersWithLimitRoute) usersWithLimitHandler fastify
  handleRoute (Proxy :: _ CreateUserRoute) createUserHandler fastify

  pure fastify

main :: Effect Unit
main = Aff.launchAff_ do
  server <- liftEffect createServer
  address <- F.listen { port: Port 3000, host: Host "0.0.0.0" } server
  Console.log $ "Server listening on " <> address
