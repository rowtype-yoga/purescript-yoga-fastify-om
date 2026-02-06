module TestNewOpenAPIFeatures where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Path (Lit, type (/), )
import Yoga.Fastify.Om.Route (Route, Request, GET, POST, JSON, route, toOpenAPI, buildOpenAPISpec', Description, Example, Format, Enum, BearerToken)
import Yoga.JSON (writeJSON)

--------------------------------------------------------------------------------
-- Enum Types
--------------------------------------------------------------------------------

data OrderStatusEnum = Pending | Processing | Shipped | Delivered | Cancelled

derive instance Generic OrderStatusEnum _
instance Show OrderStatusEnum where
  show = genericShow

data CacheControlEnum = NoCache | NoStore | MaxAge3600 | Public | Private

derive instance Generic CacheControlEnum _
instance Show CacheControlEnum where
  show = genericShow

data PriorityEnum = Low | Medium | High | Urgent

derive instance Generic PriorityEnum _
instance Show PriorityEnum where
  show = genericShow

--------------------------------------------------------------------------------
-- New Feature: Enum Support
--------------------------------------------------------------------------------

-- Enum type for order status
type OrderStatus =
  Description "Current order status"
    (Enum OrderStatusEnum)

-- Route with enum in path parameter
type GetOrdersByStatusRoute = Route
  GET
  (Lit "orders" / "status" : OrderStatus)
  (Request {})
  (ok :: { body :: Array { id :: String, status :: OrderStatus } })

-- Route with enum in request body
type UpdateOrderRoute = Route
  POST
  (Lit "orders" / "orderId" : String)
  (Request { body :: JSON { status :: OrderStatus } })
  ( ok :: { body :: { id :: String, status :: OrderStatus } }
  , badRequest :: { body :: { error :: String } }
  )

--------------------------------------------------------------------------------
-- New Feature: Response Header Metadata
--------------------------------------------------------------------------------

-- Response headers with rich metadata
type CorrelationId =
  Description "Request correlation ID for distributed tracing"
    ( Example "550e8400-e29b-41d4-a716-446655440000"
        (Format "uuid" String)
    )

type RateLimitRemaining =
  Description "Number of API requests remaining in the current rate limit window"
    (Example "95" Int)

type CacheControl =
  Description "HTTP cache control directive"
    (Enum CacheControlEnum)

-- Route with rich response headers
type GetUserRoute = Route
  GET
  (Lit "users" / "id" : String)
  (Request { headers :: { authorization :: BearerToken } })
  ( ok ::
      { body :: { id :: String, name :: String, email :: String }
      , headers ::
          { "x-correlation-id" :: CorrelationId
          , "x-rate-limit-remaining" :: RateLimitRemaining
          , "cache-control" :: CacheControl
          }
      }
  , notFound ::
      { body :: { error :: String }
      , headers :: { "x-correlation-id" :: CorrelationId }
      }
  )

--------------------------------------------------------------------------------
-- Combined Example: Enum + Response Headers
--------------------------------------------------------------------------------

type ApiKey =
  Description "API authentication key"
    ( Example "sk_live_1234567890abcdef"
        (Format "password" String)
    )

type Priority =
  Description "Task priority level"
    (Enum PriorityEnum)

type CreateTaskRoute = Route
  POST
  (Lit "tasks")
  ( Request
      { headers :: { "x-api-key" :: ApiKey }
      , body :: JSON { title :: String, priority :: Priority }
      }
  )
  ( ok ::
      { body :: { id :: String, title :: String, priority :: Priority, createdAt :: String }
      , headers ::
          { "x-correlation-id" :: CorrelationId
          , "x-rate-limit-remaining" :: RateLimitRemaining
          , "location" :: Description "URL of the created resource" String
          }
      }
  , badRequest ::
      { body :: { error :: String, field :: String }
      , headers :: { "x-correlation-id" :: CorrelationId }
      }
  )

--------------------------------------------------------------------------------
-- Main: Output OpenAPI JSON for each route
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  log "╔════════════════════════════════════════════════════════════╗"
  log "║       New OpenAPI Features Demonstration                  ║"
  log "╚════════════════════════════════════════════════════════════╝"
  log ""

  log "━━━ Feature 1: Enum Support (Path Parameter) ━━━"
  log $ toOpenAPI @GetOrdersByStatusRoute
  log ""

  log "━━━ Feature 1: Enum Support (Request Body) ━━━"
  log $ toOpenAPI @UpdateOrderRoute
  log ""

  log "━━━ Feature 2: Response Header Metadata ━━━"
  log $ toOpenAPI @GetUserRoute
  log ""

  log "━━━ Combined: Enum + Rich Response Headers ━━━"
  log $ toOpenAPI @CreateTaskRoute
  log ""

  log "━━━ Feature 3: Complete API Spec with Server Definitions ━━━"
  let
    apiSpec = buildOpenAPISpec' @(GetOrdersByStatusRoute)
      { title: "Order Management API"
      , version: "2.0.0"
      }
      { servers: Just
          [ { url: "https://api.example.com/v2"
            , description: Just "Production server"
            }
          , { url: "https://staging-api.example.com/v2"
            , description: Just "Staging server"
            }
          , { url: "http://localhost:3000/v2"
            , description: Just "Local development server"
            }
          ]
      }
  log $ writeJSON apiSpec
  log ""

  log "✓ All new OpenAPI features demonstrated successfully!"
