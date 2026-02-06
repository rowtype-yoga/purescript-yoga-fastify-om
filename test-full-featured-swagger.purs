module Test.FullFeaturedSwagger where

import Prelude

import Data.Argonaut.Core (stringify)
import Effect (Effect)
import Effect.Console as Console
import Examples.FullFeaturedAPI (fullAPISpec)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Yoga.JSON (writeJSON)

main :: Effect Unit
main = do
  Console.log "=== Full-Featured API OpenAPI Specification ==="
  Console.log ""

  let specJSON = writeJSON fullAPISpec
  let specString = stringify specJSON

  -- Write to file
  FS.writeTextFile UTF8 "full-featured-swagger.json" specString
  Console.log "✅ OpenAPI spec written to: full-featured-swagger.json"
  Console.log ""

  -- Pretty print
  Console.log "📋 Generated OpenAPI spec:"
  Console.log specString
  Console.log ""

  Console.log "🎯 Features Demonstrated:"
  Console.log "  ✅ Bearer Token Authentication (JWT)"
  Console.log "  ✅ Path Parameters with validation (minimum, pattern)"
  Console.log "  ✅ Query Parameters with pagination, sorting, filtering"
  Console.log "  ✅ Required Query Parameters (search query)"
  Console.log "  ✅ Enum Types (roles, statuses, sort orders)"
  Console.log "  ✅ String Validation (email, username patterns, lengths)"
  Console.log "  ✅ Number Constraints (minimum, maximum)"
  Console.log "  ✅ Format Annotations (email, date-time, uuid, uri)"
  Console.log "  ✅ Response Headers with Metadata"
  Console.log "  ✅ Correlation IDs for distributed tracing"
  Console.log "  ✅ Rate Limiting headers"
  Console.log "  ✅ ETags and versioning"
  Console.log "  ✅ Nullable/Optional Fields"
  Console.log "  ✅ Deprecated Fields (with warnings)"
  Console.log "  ✅ Multiple Response Status Codes"
  Console.log "  ✅ Error Responses"
  Console.log "  ✅ Multiple Server Definitions"
  Console.log "  ✅ NoBody responses (204 No Content)"
  Console.log "  ✅ Nested Resources (users/:id/orders)"
  Console.log ""

  Console.log "🚀 Routes Included:"
  Console.log "  GET    /health"
  Console.log "  GET    /users (with pagination & filtering)"
  Console.log "  GET    /users/:id"
  Console.log "  POST   /users"
  Console.log "  PUT    /users/:id"
  Console.log "  DELETE /users/:id"
  Console.log "  GET    /users/:userId/orders"
  Console.log "  GET    /users/search?query=..."
  Console.log ""

  Console.log "💡 To view in Swagger UI:"
  Console.log "  1. Visit: https://editor.swagger.io/"
  Console.log "  2. File > Import File > Select full-featured-swagger.json"
  Console.log "  3. Or paste the JSON content directly"
  Console.log ""

  Console.log "💡 To use with Swagger UI locally:"
  Console.log "  docker run -p 8080:8080 -v $(pwd):/usr/share/nginx/html/api swaggerapi/swagger-ui"
  Console.log "  Then visit: http://localhost:8080/?url=/api/full-featured-swagger.json"
