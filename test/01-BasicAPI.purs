module Test.BasicAPI where

import Prelude

import Control.Monad.Reader.Trans (ask)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object as FObject
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Fastify (Fastify, Host(..), Port(..))
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Om.API (registerAPI)
import Yoga.Fastify.Om.Route (GET, POST, Route, Request, Handler, handle, respond, reject, buildOpenAPISpec, class HeaderValueType, BearerToken, Enum, class RenderJSONSchema, class HasEnum, enum)
import Yoga.HTTP.API.Route.OpenAPI (class CollectSchemas, class CollectSchemaNames)
import Yoga.HTTP.API.Path (class ParseParam, parseParam, type (/), type (:), type (:?))
import Yoga.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Yoga.JSON.Generics (genericWriteForeignEnum, genericReadForeignEnum)
import Yoga.JSON.Generics.EnumSumRep as GenericJSON

main :: Effect Unit
main = Aff.launchAff_ do
  server <- createServer # liftEffect
  address <- F.listen { port: Port 3000, host: Host "0.0.0.0" } server
  Console.log $ "Server listening on " <> address

-- Example Types

type User =
  { id :: UserId
  , name :: String
  , email :: String
  , role :: UserRole
  }

type CreateUserRequest =
  { name :: String
  , email :: String
  , role :: UserRole
  }

type ErrorResponse = { error :: String }

-- Routes

-- GET /health
type HealthRoute = Route GET "health"
  (Request {})
  ( ok :: { body :: { status :: String } }
  , notFound :: { body :: ErrorResponse }
  )

healthHandler :: Handler HealthRoute ()
healthHandler = handle do
  respond @"ok" { status: "healthy" }

newtype UserId = UserId Int

derive instance Newtype UserId _
derive newtype instance Eq UserId
derive newtype instance WriteForeign UserId
derive newtype instance ReadForeign UserId

instance RenderJSONSchema UserId where
  renderJSONSchema _ = unsafeToForeign $ FObject.fromFoldable
    [ Tuple "type" (unsafeToForeign "integer") ]

instance CollectSchemas UserId where
  collectSchemas _ = FObject.empty

instance CollectSchemaNames UserId ()

instance ParseParam UserId where
  parseParam s = parseParam s >>= \n ->
    if n > 0 then Right (UserId n)
    else Left "UserId must be positive"

instance HeaderValueType UserId where
  headerValueType _ = "integer"

-- GET /users/:id (with authentication)
type UserRoute = Route GET
  ("users" / "id" : UserId)
  (Request { headers :: { authorization :: BearerToken } })
  ( ok :: { body :: User }
  , notFound :: { body :: ErrorResponse }
  , unauthorized :: { body :: ErrorResponse }
  )

userHandler :: Handler UserRoute ()
userHandler = handle do
  { path } <- ask

  when (path.id /= UserId 1) do
    reject @404 { error: "User not found" }

  respond @200
    { id: UserId 1
    , name: "Alice"
    , email: "alice@example.com"
    , role: Admin
    }

-- GET /users?limit=10
type UsersWithLimitRoute = Route GET
  ("users" :? { limit :: Int })
  (Request {})
  ( ok :: { body :: { users :: Array User, limit :: Maybe Int } }
  )

usersWithLimitHandler :: Handler UsersWithLimitRoute ()
usersWithLimitHandler = handle do
  { query } <- ask
  respond @"ok"
    { users:
        [ { id: UserId 1, name: "Alice", email: "alice@example.com", role: Admin }
        , { id: UserId 2, name: "Bob", email: "bob@example.com", role: Member }
        ]
    , limit: query.limit
    }

-- POST /users (with authentication)
type CreateUserRoute = Route POST
  "users"
  ( Request
      { headers :: { authorization :: BearerToken }
      , body :: CreateUserRequest
      }
  )
  ( created :: { body :: User }
  , badRequest :: { body :: ErrorResponse }
  , unauthorized :: { body :: ErrorResponse }
  )

createUserHandler :: Handler CreateUserRoute ()
createUserHandler = handle do
  { body } <- ask
  when (body.name == "") do
    reject @400 { error: "Name cannot be empty" }
  respond @201
    { id: UserId 999
    , name: body.name
    , email: body.email
    , role: body.role
    }

-- GET /openapi - Serve OpenAPI spec
type OpenAPIRoute = Route GET
  "openapi"
  (Request {})
  ( ok :: { body :: String }
  )

-- Define all API routes for OpenAPI generation
type AllApiRoutes =
  { health :: HealthRoute
  , user :: UserRoute
  , usersWithLimit :: UsersWithLimitRoute
  , createUser :: CreateUserRoute
  }

openapiHandler :: Handler OpenAPIRoute ()
openapiHandler = handle do
  respond @"ok"
    $ writeJSON
    $
      buildOpenAPISpec @AllApiRoutes
        { title: "Example API"
        , version: "1.0.0"
        }

-- Server Setup

createServer :: Effect Fastify
createServer = do
  fastify <- F.fastify {}

  registerAPI
    { health: healthHandler
    , user: userHandler
    , usersWithLimit: usersWithLimitHandler
    , createUser: createUserHandler
    , openapi: openapiHandler
    }
    fastify

  pure fastify

-- User role enum
data UserRole = Admin | Member | Guest

derive instance Generic UserRole _
instance Show UserRole where
  show = genericShow

instance WriteForeign UserRole where
  writeImpl = genericWriteForeignEnum GenericJSON.defaultOptions

instance ReadForeign UserRole where
  readImpl = genericReadForeignEnum GenericJSON.defaultOptions

instance RenderJSONSchema UserRole where
  renderJSONSchema _ = do
    let
      enumValues = enum (Proxy :: Proxy (Enum UserRole))
      baseSchema = FObject.fromFoldable
        [ Tuple "type" (unsafeToForeign "string")
        ]
    case enumValues of
      Nothing -> unsafeToForeign baseSchema
      Just vals -> unsafeToForeign $ FObject.insert "enum" (unsafeToForeign vals) baseSchema

instance CollectSchemas UserRole where
  collectSchemas _ = FObject.empty

instance CollectSchemaNames UserRole ()
