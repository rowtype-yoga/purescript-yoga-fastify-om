module Test.CompileFail.RegisterAPILayerComplexHappyPath where

import Prelude

import Yoga.Fastify.Fastify (Fastify)
import Yoga.Fastify.Om.API (registerAPILayer)
import Yoga.Fastify.Om.Route (GET, Handler, Route, handle, respond)
import Yoga.Om.Layer (OmLayer)

data Connection = Connection

data Logger = Logger

data Metrics = Metrics

type HealthRoute = Route GET "health" {} (ok :: { body :: { status :: String } })
type MetricsRoute = Route GET "metrics" {} (ok :: { body :: { enabled :: Boolean } })
type AdminRoute = Route GET "admin" {} (ok :: { body :: { connected :: Boolean } })

type API =
  { health :: HealthRoute
  , metrics :: MetricsRoute
  , admin :: AdminRoute
  }

healthHandler :: Handler HealthRoute (logger :: Logger, sqlite :: Connection)
healthHandler = handle do
  respond @"ok" { status: "healthy" }

metricsHandler :: Handler MetricsRoute (metrics :: Metrics)
metricsHandler = handle do
  respond @"ok" { enabled: true }

adminHandler :: Handler AdminRoute (logger :: Logger, sqlite :: Connection, metrics :: Metrics)
adminHandler = handle do
  respond @"ok" { connected: true }

apiLayer :: OmLayer (fastify :: Fastify, logger :: Logger, sqlite :: Connection, metrics :: Metrics) () {}
apiLayer = registerAPILayer @API
  { health: healthHandler
  , metrics: metricsHandler
  , admin: adminHandler
  }
