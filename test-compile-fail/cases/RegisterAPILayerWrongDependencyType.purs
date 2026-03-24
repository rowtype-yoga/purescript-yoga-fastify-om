-- EXPECT: while matching label sqlite
module Test.CompileFail.RegisterAPILayerWrongDependencyType where

import Prelude

import Yoga.Fastify.Fastify (Fastify)
import Yoga.Fastify.Om.API (registerAPILayer)
import Yoga.Fastify.Om.Route (GET, Handler, Route, handle, respond)
import Yoga.Om.Layer (OmLayer)

data Connection = Connection

data Logger = Logger

type HealthRoute = Route GET "health" {} (ok :: { body :: String })

type API =
  { health :: HealthRoute
  }

healthHandler :: Handler HealthRoute (logger :: Logger, sqlite :: Connection)
healthHandler = handle do
  respond @"ok" "healthy"

apiLayer :: OmLayer (fastify :: Fastify, logger :: Logger, sqlite :: String) () {}
apiLayer = registerAPILayer @API
  { health: healthHandler
  }
