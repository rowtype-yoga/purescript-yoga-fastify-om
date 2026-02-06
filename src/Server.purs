module Example.Server where

import Prelude

import Control.Monad.Reader.Trans (ask)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign (Foreign)
import Foreign.Object as FObject
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Fastify.Fastify (Fastify, Host(..), Port(..))
import Yoga.Fastify.Fastify as F
import Yoga.HTTP.API.Path (class ParseParam, parseParam, type (/), type (:), type (:?))
import Yoga.Fastify.Om.Route (GET, POST, Route, Request, Handler, JSON, handleRoute, handle, respond, reject, buildOpenAPISpec', class HeaderValueType, BearerToken, Enum, class RenderJSONSchema, class HasEnum, enum)
import Yoga.JSON (writeJSON, class WriteForeign, class ReadForeign)
import Yoga.JSON.Generics (genericWriteForeignEnum, genericReadForeignEnum)

--------------------------------------------------------------------------------
-- Example Types
--------------------------------------------------------------------------------

-- User role enum
data UserRole = Admin | Member | Guest

derive instance Generic UserRole _
instance Show UserRole where
  show = genericShow

instance WriteForeign UserRole where
  writeImpl = genericWriteForeignEnum { toConstructorName: identity }

instance ReadForeign UserRole where
  readImpl = genericReadForeignEnum { toConstructorName: identity }

instance RenderJSONSchema UserRole where
  renderJSONSchema _ =
    let
      enumValues = enum (Proxy :: Proxy (Enum UserRole))
      baseSchema = FObject.fromFoldable
        [ unsafeCoerce $ { key: "type", value: unsafeCoerce "string" }
        ]
    in
      case enumValues of
        Nothing -> unsafeCoerce baseSchema
        Just vals -> unsafeCoerce $ FObject.insert "enum" (unsafeCoerce vals) baseSchema

type User =
  { id :: Int
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

--------------------------------------------------------------------------------
-- Routes
--------------------------------------------------------------------------------

-- GET /health
type HealthRoute = Route GET
  "health"
  (Request {})
  ( ok :: { body :: { status :: String } }
  , notFound :: { body :: ErrorResponse }
  )

healthHandler :: Handler HealthRoute
healthHandler = handle do
  respond { ok: { status: "healthy" } }

newtype UserId = UserId Int

derive instance Newtype UserId _
derive newtype instance Eq UserId

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

userHandler :: Handler UserRoute
userHandler = handle do
  { path } <- ask
  when (path.id /= UserId 1) do
    reject { notFound: { error: "User not found" } }
  respond { ok: { id: 1, name: "Alice", email: "alice@example.com", role: Admin } }

-- GET /users?limit=10
type UsersWithLimitRoute = Route GET
  ("users" :? { limit :: Int })
  (Request {})
  ( ok :: { body :: { users :: Array User, limit :: Maybe Int } }
  )

usersWithLimitHandler :: Handler UsersWithLimitRoute
usersWithLimitHandler = handle do
  { query } <- ask
  respond
    { ok:
        { users:
            [ { id: 1, name: "Alice", email: "alice@example.com", role: Admin }
            , { id: 2, name: "Bob", email: "bob@example.com", role: Member }
            ]
        , limit: query.limit
        }
    }

-- POST /users (with authentication)
type CreateUserRoute = Route POST
  "users"
  ( Request
      { headers :: { authorization :: BearerToken }
      , body :: JSON CreateUserRequest
      }
  )
  ( created :: { body :: User }
  , badRequest :: { body :: ErrorResponse }
  , unauthorized :: { body :: ErrorResponse }
  )

createUserHandler :: Handler CreateUserRoute
createUserHandler = handle do
  { body } <- ask
  when (body.name == "") do
    reject { badRequest: { error: "Name cannot be empty" } }
  respond { created: { id: 999, name: body.name, email: body.email, role: body.role } }

-- GET /openapi - Serve OpenAPI spec
type OpenAPIRoute = Route GET
  "openapi"
  (Request {})
  ( ok :: { body :: String }
  )

-- Define all API routes for OpenAPI generation
type AllApiRoutes =
  HealthRoute
    /\ UserRoute
    /\ UsersWithLimitRoute
    /\
      CreateUserRoute

openapiHandler :: Handler OpenAPIRoute
openapiHandler = handle do
  respond
    { ok:
        writeJSON $
          buildOpenAPISpec' @AllApiRoutes
            { title: "Example API"
            , version: "1.0.0"
            }
            { servers: Just
                [ { url: "http://localhost:3000"
                  , description: Just "Local development server"
                  }
                , { url: "https://api.example.com"
                  , description: Just "Production server"
                  }
                ]
            }
    }

--------------------------------------------------------------------------------
-- Server Setup
--------------------------------------------------------------------------------

createServer :: Effect Fastify
createServer = do
  fastify <- F.fastify {}

  -- Register API routes
  handleRoute healthHandler fastify
  handleRoute userHandler fastify
  handleRoute usersWithLimitHandler fastify
  handleRoute createUserHandler fastify

  -- Register OpenAPI spec endpoint
  handleRoute openapiHandler fastify

  pure fastify

main :: Effect Unit
main = Aff.launchAff_ do
  server <- liftEffect createServer
  address <- F.listen { port: Port 3000, host: Host "0.0.0.0" } server
  Console.log $ "Server listening on " <> address
