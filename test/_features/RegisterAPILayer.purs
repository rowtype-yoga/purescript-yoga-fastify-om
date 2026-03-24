module Test.Features.RegisterAPILayer where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)
import Yoga.Fastify.Fastify (Fastify)
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Om.API (registerAPILayer)
import Yoga.Fastify.Om.Route (GET, Handler, Route, handle, respond)
import Yoga.Om (runOm)
import Yoga.Om.Layer (OmLayer, runLayer)

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

testRegisterAPILayer :: Effect ViTest
testRegisterAPILayer = describe "registerAPILayer" do
  test "accepts handlers whose dependencies are subsets of the layer context" do
    app <- liftEffect $ F.fastify {}
    let ctx = { fastify: app, logger: Logger, sqlite: Connection, metrics: Metrics }
    Aff.finally (F.close app) do
      _ <- runOm ctx { exception: Aff.throwError } $ runLayer ctx apiLayer
      expectToBe true true
