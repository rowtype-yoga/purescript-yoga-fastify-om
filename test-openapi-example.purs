module TestOpenAPIExample where

import Prelude
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Om.Route (GET, POST, Route, route, toOpenAPI, Request, JSON, BearerToken)
import Yoga.HTTP.API.Path (Lit, Capture, type (/), type (:?), Required)
import Effect (Effect)
import Effect.Console (log)

-- Simple route with no params
type SimpleRoute = Route GET (Lit "users") (Request {}) (ok :: { body :: JSON (Array String) })

-- Route with path capture
type CaptureRoute = Route GET (Lit "users" / Capture "id" String) (Request {}) (ok :: { body :: JSON String })

-- Route with headers
type HeaderRoute = Route GET (Lit "users") (Request { headers :: { authorization :: BearerToken } }) (ok :: { body :: JSON (Array String) })

-- Route with query params
type QueryRoute = Route GET (Lit "users" :? (limit :: Int, offset :: Required Int)) (Request {}) (ok :: { body :: JSON (Array String) })

-- Route with multiple responses
type MultiResponseRoute = Route POST (Lit "users") 
  (Request { body :: JSON { name :: String } }) 
  ( ok :: { body :: JSON { id :: Int } }
  , badRequest :: { body :: JSON { error :: String } }
  )

main :: Effect Unit
main = do
  log "=== Simple route ==="
  log $ toOpenAPI @SimpleRoute
  log ""
  
  log "=== Route with path capture ==="
  log $ toOpenAPI @CaptureRoute
  log ""
  
  log "=== Route with headers ==="
  log $ toOpenAPI @HeaderRoute
  log ""
  
  log "=== Route with query params ==="
  log $ toOpenAPI @QueryRoute
  log ""
  
  log "=== Route with multiple responses ==="
  log $ toOpenAPI @MultiResponseRoute
