-- EXPECT: sqlite
module Test.CompileFail.RegisterAPILayerMissingSqliteDependency where

import Prelude

import Yoga.Fastify.Fastify (Fastify)
import Yoga.Fastify.Om.API (registerAPILayer)
import Yoga.Fastify.Om.Route (GET, Handler, Route, handle, respond)
import Yoga.Om.Layer (OmLayer)

data Connection = Connection

type HealthRoute = Route GET "health" {} (ok :: { body :: { status :: String } })

type API =
  { health :: HealthRoute
  }

healthHandler :: Handler HealthRoute (sqlite :: Connection)
healthHandler = handle do
  respond @"ok" { status: "healthy" }

apiLayer :: OmLayer (fastify :: Fastify) () {}
apiLayer = registerAPILayer @API
  { health: healthHandler
  }
