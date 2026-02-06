module TestSecurityExample where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested (type (/\))
import Yoga.Fastify.Om.Route (GET, POST, Route, Request, JSON, BearerToken, buildOpenAPISpec, Description)
import Yoga.HTTP.API.Path (type (:))
import Effect (Effect)
import Effect.Console (log)
import Yoga.JSON (writeJSON)
import Type.Function (type (#))

-- Route without authentication
type PublicRoute = Route GET
  "public"
  (Request {})
  (ok :: { body :: { message :: String } })

-- Route with bearer token authentication
type AuthToken = BearerToken # Description "JWT token for authentication"

type ProtectedRoute = Route GET
  "protected"
  (Request { headers :: { authorization :: AuthToken } })
  ( ok :: { body :: { secret :: String } }
  , unauthorized :: { body :: { error :: String } }
  )

-- Route with plain BearerToken (no description)
type ProtectedRoute2 = Route POST
  "admin"
  ( Request
      { headers :: { authorization :: BearerToken }
      , body :: JSON { action :: String }
      }
  )
  ( ok :: { body :: { result :: String } }
  , unauthorized :: { body :: { error :: String } }
  , forbidden :: { body :: { error :: String } }
  )

-- Mix of public and protected routes
type API = PublicRoute /\ ProtectedRoute /\ ProtectedRoute2

main :: Effect Unit
main = do
  log "=== OpenAPI Spec with Security Schemes ==="
  log ""
  let spec = buildOpenAPISpec @API { title: "Secure API", version: "1.0.0" }
  log $ writeJSON spec
  log ""
  log "=== Done ==="
