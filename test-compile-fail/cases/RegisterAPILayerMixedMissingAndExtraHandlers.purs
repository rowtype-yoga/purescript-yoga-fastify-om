-- EXPECT: ghost
module Test.CompileFail.RegisterAPILayerMixedMissingAndExtraHandlers where

import Prelude

import Yoga.Fastify.Fastify (Fastify)
import Yoga.Fastify.Om.API (registerAPILayer)
import Yoga.Fastify.Om.Route (GET, Handler, Route, handle, respond)
import Yoga.Om.Layer (OmLayer)

data Connection = Connection

type HealthRoute = Route GET "health" {} (ok :: { body :: String })
type UsersRoute = Route GET "users" {} (ok :: { body :: { count :: Int } })

type API =
  { health :: HealthRoute
  , users :: UsersRoute
  }

healthHandler :: Handler HealthRoute ()
healthHandler = handle do
  respond @"ok" "healthy"

usersHandler :: Handler UsersRoute (sqlite :: Connection)
usersHandler = handle do
  respond @"ok" { count: 0 }

ghostHandler :: Handler HealthRoute ()
ghostHandler = handle do
  respond @"ok" "ghost"

apiLayer :: OmLayer (fastify :: Fastify) () {}
apiLayer = registerAPILayer @API
  { health: healthHandler
  , ghost: ghostHandler
  }
