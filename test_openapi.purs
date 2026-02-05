module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Om.Path (Root)
import Yoga.Fastify.Om.Route (Route, ResponseData, GET, POST, toOpenAPI)

type SimpleRoute = Route GET Root () (ok :: ResponseData () String)
type RouteWithHeaders = Route POST Root
  (authorization :: String)
  (created :: ResponseData ("Location" :: String, "X-Request-Id" :: String) Unit)

main :: Effect Unit
main = do
  log "Simple route OpenAPI:"
  log $ toOpenAPI (Proxy :: Proxy SimpleRoute)
  log "\n\nRoute with request and response headers:"
  log $ toOpenAPI (Proxy :: Proxy RouteWithHeaders)
