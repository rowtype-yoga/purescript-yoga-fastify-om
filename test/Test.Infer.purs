module Test.Infer where

import Prelude

import Control.Monad.Reader (asks)
import Control.Monad.Reader.Trans (ask)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Yoga.Om.Ref as Ref
import Foreign (unsafeToForeign)
import Foreign.Object as FObject
import Type.Function (type (#))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Fastify.Fastify (Fastify, Host(..), Port(..), RouteURL(..))
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Om.API (registerAPILayer)
import Yoga.Fastify.Om.Route (class HeaderValueType, class RenderJSONSchema, GET, Handler, PUT, Route, buildOpenAPISpec, handle, mapReject, onError, respond)
import Yoga.HTTP.API.Path (type (/), type (:))
import Yoga.HTTP.API.Route (class ParseParam)
import Yoga.HTTP.API.Route.OpenAPI (class CollectSchemaNames, class CollectSchemas)
import Yoga.HTTP.API.Route.OpenAPIMetadata (Format, MinLength)
import Yoga.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Yoga.Om (Om, toOm)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, runLayer)

type User = { name :: UserName, email :: UserEmail }
newtype UserName = UserName (String # MinLength 3)

derive newtype instance ParseParam UserName
derive newtype instance WriteForeign UserName
derive newtype instance HeaderValueType UserName
derive newtype instance RenderJSONSchema UserName
derive newtype instance CollectSchemas UserName
instance CollectSchemaNames UserName ()
instance Show UserName where
  show = unsafeCoerce :: UserName -> String

newtype UserEmail = UserEmail (String # Format "email")

derive newtype instance WriteForeign UserEmail
derive newtype instance ReadForeign UserEmail
derive newtype instance RenderJSONSchema UserEmail
derive newtype instance CollectSchemas UserEmail
instance CollectSchemaNames UserEmail ()

type UserRepo = { findByName :: UserName -> Aff (Maybe User), upsertUser :: User -> Aff Unit }

type PutUser = Route
  PUT
  ("users" / "name" : UserName)
  { body :: { email :: UserEmail } }
  ( ok :: { body :: User }
  , created :: { body :: User }
  )

type GetUser = Route GET
  ("users" / "name" : UserName)
  {}
  ( ok :: { body :: User }
  , notFound :: { body :: { error :: String } }
  )

type GetHealth = Route GET "health" {} (ok :: { body :: { status :: String } })

type OpenAPIRoute = Route GET "openapi" {} (ok :: { body :: String })

type API =
  { getUser :: GetUser
  , putUser :: PutUser
  , health :: GetHealth
  , openapi :: OpenAPIRoute
  }

getUserHandler :: Handler GetUser (userRepo :: UserRepo)
getUserHandler = handle do
  { path } <- ask
  user <- findUserByName path.name
    # mapReject @"userNotFound" @404 \name ->
        { error: "User not found: " <> show name }
  respond @"ok" user

putUserHandler :: Handler PutUser (userRepo :: UserRepo)
putUserHandler = handle do
  { path, body } <- ask
  let user = { name: path.name, email: body.email }
  _ <- (Just <$> findUserByName user.name)
    # onError @"userNotFound" (const $ pure Nothing)
  upsertUser user
  respond @"ok" user

healthHandler :: Handler GetHealth ()
healthHandler = handle do
  respond @200 { status: "healthy" }

openapiHandler :: Handler OpenAPIRoute ()
openapiHandler = handle do
  respond @"ok" $ writeJSON $ buildOpenAPISpec @API
    { title: "Test.Infer API"
    , version: "1.0.0"
    }

apiLayer :: OmLayer (fastify :: Fastify, userRepo :: UserRepo) () {}
apiLayer = registerAPILayer @API
  { getUser: getUserHandler
  , putUser: putUserHandler
  , health: healthHandler
  , openapi: openapiHandler
  }

type UserRepoCtx r = (userRepo :: UserRepo | r)

findUserByName :: forall ctx err. UserName -> Om { | UserRepoCtx ctx } (userNotFound :: UserName | err) User
findUserByName name = do
  repo <- asks _.userRepo
  repo.findByName name # toOm >>= case _ of
    Just user -> pure user
    Nothing -> Om.throw { userNotFound: name }

upsertUser :: forall ctx err. User -> Om { | UserRepoCtx ctx } err Unit
upsertUser user = do
  repo <- asks _.userRepo
  repo.upsertUser user # toOm

-- In-memory user repo for testing
mkUserRepo :: Effect UserRepo
mkUserRepo = do
  usersRef <- Ref.new (FObject.empty :: FObject.Object User)
  let toKey = unsafeCoerce :: UserName -> String
  pure
    { findByName: \name -> do
        users <- Ref.read usersRef
        pure (FObject.lookup (toKey name) users)
    , upsertUser: \user ->
        Ref.modify_ (FObject.insert (toKey user.name) user) usersRef
    }

main :: Effect Unit
main = Aff.launchAff_ do
  fastify <- F.fastify {} # liftEffect
  userRepo <- mkUserRepo # liftEffect
  let ctx = { fastify, userRepo }
  _ <- Om.runOm ctx { exception: Aff.throwError } $ runLayer ctx apiLayer
  liftEffect $ F.get (RouteURL "/docs") docsHandler fastify
  address <- F.listen { port: Port 3000, host: Host "0.0.0.0" } fastify
  Console.log $ "Server listening on " <> address
  Console.log $ "  API docs: http://localhost:3000/docs"

docsHandler :: F.FastifyRequest -> F.FastifyReply -> Aff Unit
docsHandler _req reply = do
  void $ F.header "content-type" "text/html" reply # liftEffect
  F.send (unsafeToForeign docsHtml) reply

docsHtml :: String
docsHtml =
  """<!doctype html>
<html>
  <head>
    <title>API Reference</title>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
  </head>
  <body>
    <div id="app"></div>
    <script src="https://cdn.jsdelivr.net/npm/@scalar/api-reference"></script>
    <script>
      Scalar.createApiReference('#app', { url: '/openapi' })
    </script>
  </body>
</html>"""
