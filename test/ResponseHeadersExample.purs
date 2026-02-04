module Test.Yoga.Fastify.Om.ResponseHeadersExample where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG
import Routing.Duplex.Generic.Syntax ((/))
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Om.Endpoint as E
import Yoga.Fastify.Om.RequestBody (RequestBody(..))
import Yoga.JSON (class ReadForeign, class WriteForeign)

type User =
  { id :: Int
  , name :: String
  , email :: String
  }

type CreateUserReq =
  { name :: String
  , email :: String
  }

data ApiRoute = CreateUser | GetUser

derive instance Generic ApiRoute _
derive instance Eq ApiRoute

apiRoute :: RouteDuplex' ApiRoute
apiRoute = RD.root $ RG.sum
  { "CreateUser": "users" / RG.noArgs
  , "GetUser": "users" / "1" / RG.noArgs
  }

type AppContext = ()

type CreateUserRequest = { body :: RequestBody CreateUserReq }
type CreateUserResponse = { body :: User }

createUserHandler :: E.EndpointHandler AppContext () ApiRoute CreateUserRequest CreateUserResponse
createUserHandler { path, request } =
  case path, request.body of
    CreateUser, JSONBody { name, email } -> pure { body: { id: 1, name, email } }
    _, _ -> pure { body: { id: 0, name: "error", email: "error" } }

type ListUsersRequest =
  { query :: Record (page :: Int, limit :: Int)
  , body :: RequestBody Unit
  }

type ListUsersResponse = { body :: Array User }

listUsersHandler :: E.EndpointHandler AppContext () ApiRoute ListUsersRequest ListUsersResponse
listUsersHandler { request } = do
  let { page, limit } = request.query
  pure
    { body:
        [ { id: page, name: "Alice", email: "alice@example.com" }
        , { id: limit, name: "Bob", email: "bob@example.com" }
        ]
    }

type AuthenticatedCreateRequest =
  { headers :: Record (authorization :: String)
  , body :: RequestBody CreateUserReq
  }

type AuthenticatedCreateResponse =
  E.Response ("Location" :: String, "X-Request-Id" :: String) User

authenticatedCreateHandler
  :: E.EndpointHandler AppContext () ApiRoute AuthenticatedCreateRequest AuthenticatedCreateResponse
authenticatedCreateHandler { path, request } =
  case path, request.body of
    CreateUser, JSONBody { name, email } -> do
      let authToken = request.headers.authorization
      let userId = 123
      pure
        { status: E.StatusCode 201
        , headers:
            { "Location": "/users/" <> show userId
            , "X-Request-Id": authToken
            }
        , body: { id: userId, name, email }
        }
    _, _ -> pure
      { status: E.StatusCode 400
      , headers: { "Location": "", "X-Request-Id": "req-error" }
      , body: { id: 0, name: "error", email: "error" }
      }

type FullCreateRequest =
  { query :: Record (dryRun :: Maybe Boolean)
  , headers :: Record (authorization :: String)
  , body :: RequestBody CreateUserReq
  }

type FullCreateResponse =
  E.Response
    ( "Location" :: String
    , "X-Request-Id" :: String
    , "Content-Type" :: String
    , "Cache-Control" :: String
    )
    User

fullCreateHandler
  :: E.EndpointHandler AppContext () ApiRoute FullCreateRequest FullCreateResponse
fullCreateHandler { path, request } =
  case path, request.body of
    CreateUser, JSONBody { name, email } -> do
      let
        userId = case request.query.dryRun of
          Just true -> 0
          _ -> 123
      let requestId = request.headers.authorization
      pure
        { status: E.StatusCode 201
        , headers:
            { "Location": "/users/" <> show userId
            , "X-Request-Id": requestId
            , "Content-Type": "application/json; charset=utf-8"
            , "Cache-Control": "no-cache"
            }
        , body: { id: userId, name, email }
        }
    _, _ -> pure
      { status: E.StatusCode 400
      , headers:
          { "Location": ""
          , "X-Request-Id": "req-error"
          , "Content-Type": "application/json"
          , "Cache-Control": "no-cache"
          }
      , body: { id: 0, name: "error", email: "error" }
      }

type GetUserResponse = { body :: User }

getUserHandler :: E.EndpointHandler AppContext () ApiRoute { body :: RequestBody Unit } GetUserResponse
getUserHandler { path } =
  case path of
    GetUser -> pure { body: { id: 1, name: "Alice", email: "alice@example.com" } }
    _ -> pure { body: { id: 0, name: "Bad Request", email: "" } }
