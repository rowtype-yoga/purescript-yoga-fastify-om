-- EXPECT: sqlite
module Test.CompileFail.RegisterAPILayerMissingDependencyAcrossMultipleHandlers where

import Prelude

import Yoga.Fastify.Fastify (Fastify)
import Yoga.Fastify.Om.API (registerAPILayer)
import Yoga.Fastify.Om.Route (GET, Handler, Route, handle, respond)
import Yoga.Om.Layer (OmLayer)

data Connection = Connection

data Logger = Logger

type HealthRoute = Route GET "health" {} (ok :: { body :: { status :: String } })
type MetricsRoute = Route GET "metrics" {} (ok :: { body :: { enabled :: Boolean } })

type API =
  { health :: HealthRoute
  , metrics :: MetricsRoute
  }

healthHandler :: Handler HealthRoute (logger :: Logger, sqlite :: Connection)
healthHandler = handle do
  respond @"ok" { status: "healthy" }

metricsHandler :: Handler MetricsRoute (logger :: Logger)
metricsHandler = handle do
  respond @"ok" { enabled: true }

apiLayer :: OmLayer (fastify :: Fastify, logger :: Logger) () {}
apiLayer = registerAPILayer @API
  { health: healthHandler
  , metrics: metricsHandler
  }
