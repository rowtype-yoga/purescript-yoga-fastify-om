module Test.CompileFail.RegisterAPILayerExtraHandler where

import Prelude

import Yoga.Fastify.Fastify (Fastify)
import Yoga.Fastify.Om.API (registerAPILayer)
import Yoga.Fastify.Om.Route (GET, Handler, Route, handle, respond)
import Yoga.Om.Layer (OmLayer)

type HealthRoute = Route GET "health" {} (ok :: { body :: String })

type API =
  { health :: HealthRoute
  }

healthHandler :: Handler HealthRoute ()
healthHandler = handle do
  respond @"ok" "ok"

extraHandler :: Handler HealthRoute ()
extraHandler = handle do
  respond @"ok" "extra"

apiLayer :: OmLayer (fastify :: Fastify) () {}
apiLayer = registerAPILayer @API
  { health: healthHandler
  , extra: extraHandler
  }
