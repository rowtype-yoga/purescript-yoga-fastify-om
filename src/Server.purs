module Example.Server where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console

import Yoga.Fastify.Fastify (Fastify, Host(..), Port(..))
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Om.Path (type (/), type (:?), Capture, Path)
import Control.Monad.Reader.Trans (ask)
import Yoga.Fastify.Om.Route (GET, POST, Route, Request, Handler, mkHandler, JSON, ResponseData, handleRoute, respondNoHeaders, handle, respond, reject)

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
  (Request ())
  ( ok :: ResponseData () { status :: String }
  )

healthHandler :: Handler HealthRoute
healthHandler = mkHandler \_ -> pure $ respondNoHeaders @"ok" { status: "healthy" }

-- GET /users/:id
type UserRoute = Route GET
  (Path ("users" / Capture "id" Int))
  (Request ())
  ( ok :: ResponseData () User
  , notFound :: ResponseData () ErrorResponse
  )

userHandler :: Handler UserRoute
userHandler = mkHandler \{ path } ->
  if path.id == 1 then
    pure $ respondNoHeaders @"ok"
      { id: 1, name: "Alice", email: "alice@example.com" }
  else
    pure $ respondNoHeaders @"notFound"
      { error: "User not found" }

-- GET /users/:id (Om version — short-circuiting style)
userHandlerOm :: Handler UserRoute
userHandlerOm = handle do
  { path } <- ask
  when (path.id /= 1) $
    reject { notFound: { error: "User not found" } }
  respond { ok: { id: 1, name: "Alice", email: "alice@example.com" } }

-- GET /users?limit=10
type UsersWithLimitRoute = Route GET
  (Path "users" :? { limit :: Int })
  (Request ())
  ( ok :: ResponseData () { users :: Array User, limit :: Maybe Int }
  )

usersWithLimitHandler :: Handler UsersWithLimitRoute
usersWithLimitHandler = mkHandler \{ query } -> pure $ respondNoHeaders @"ok"
  { users:
      [ { id: 1, name: "Alice", email: "alice@example.com" }
      , { id: 2, name: "Bob", email: "bob@example.com" }
      ]
  , limit: query.limit
  }

-- POST /users
type CreateUserRoute = Route POST
  (Path "users")
  (Request (body :: JSON CreateUserRequest))
  ( created :: ResponseData () User
  , badRequest :: ResponseData () ErrorResponse
  )

createUserHandler :: Handler CreateUserRoute
createUserHandler = mkHandler \{ body } ->
  if body.name == "" then
    pure $ respondNoHeaders @"badRequest"
      { error: "Name cannot be empty" }
  else
    pure $ respondNoHeaders @"created"
      { id: 999, name: body.name, email: body.email }

--------------------------------------------------------------------------------
-- Server Setup
--------------------------------------------------------------------------------

createServer :: Effect Fastify
createServer = do
  fastify <- F.fastify {}

  -- Register routes
  handleRoute healthHandler fastify
  handleRoute userHandler fastify
  handleRoute usersWithLimitHandler fastify
  handleRoute createUserHandler fastify

  pure fastify

main :: Effect Unit
main = Aff.launchAff_ do
  server <- liftEffect createServer
  address <- F.listen { port: Port 3000, host: Host "0.0.0.0" } server
  Console.log $ "Server listening on " <> address
