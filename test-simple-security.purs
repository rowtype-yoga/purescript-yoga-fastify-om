module TestSimpleSecurity where

import Prelude
import Effect (Effect)
import Effect.Console as Console
import Yoga.Fastify.Om.Route (GET, Route, Request, BearerToken, buildOpenAPISpec)
import Yoga.JSON (writeJSON)

type ProtectedRoute = Route GET
  "protected"
  (Request { headers :: { authorization :: BearerToken } })
  (ok :: { body :: { message :: String } })

main :: Effect Unit
main = do
  Console.log "=== Single Protected Route ==="
  let spec = buildOpenAPISpec @ProtectedRoute { title: "Test API", version: "1.0.0" }
  Console.log $ writeJSON spec
