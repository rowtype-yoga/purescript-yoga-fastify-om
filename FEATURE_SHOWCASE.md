# 🎯 Complete OpenAPI Feature Showcase

This document demonstrates **all OpenAPI features** supported by `purescript-yoga-fastify-om`, including the three major features added recently: **Enum Support**, **Response Header Metadata**, and **Server Definitions**.

---

## 📚 Table of Contents

1. [Complete Feature List](#complete-feature-list)
2. [Full-Featured Example API](#full-featured-example-api)
3. [Feature Demonstrations](#feature-demonstrations)
4. [Generated OpenAPI Output](#generated-openapi-output)
5. [Using the Generated Spec](#using-the-generated-spec)

---

## ✅ Complete Feature List

### Authentication & Security
- ✅ **Bearer Token Authentication** (JWT)
- ✅ **Automatic Security Scheme Detection**
- ✅ **Security Requirements per Route**

### Path Parameters
- ✅ **Type-Safe Path Captures** (`:id`, `:userId`, etc.)
- ✅ **Validation Constraints** (minimum, maximum, pattern)
- ✅ **Format Annotations** (uuid, email, etc.)
- ✅ **Rich Descriptions and Examples**

### Query Parameters
- ✅ **Optional Query Parameters** (default)
- ✅ **Required Query Parameters** (`Required` wrapper)
- ✅ **Pagination Support** (page, pageSize)
- ✅ **Sorting and Filtering**
- ✅ **Default Values**
- ✅ **Enum Constraints** 🆕

### Request Headers
- ✅ **Custom Headers** with validation
- ✅ **Authorization Headers**
- ✅ **Conditional Headers** (If-Match, If-None-Match)
- ✅ **Full Metadata Support** (description, example, format, deprecated)

### Response Headers
- ✅ **Custom Response Headers** 🆕
- ✅ **Correlation IDs** for distributed tracing 🆕
- ✅ **Rate Limiting Headers** 🆕
- ✅ **Cache Control Headers** 🆕
- ✅ **ETag and Versioning Headers** 🆕
- ✅ **Location Headers** (for created resources) 🆕
- ✅ **Full Metadata** (description, example, format, enum, deprecated) 🆕

### Request/Response Bodies
- ✅ **JSON Bodies** with full type introspection
- ✅ **NoBody** (for 204 responses)
- ✅ **Nested Objects and Arrays**
- ✅ **Optional/Nullable Fields**
- ✅ **Rich Field Metadata** (description, example, format)

### Validation & Constraints
- ✅ **String Constraints** (minLength, maxLength, pattern)
- ✅ **Number Constraints** (minimum, maximum)
- ✅ **Enum/Union Types** 🆕
- ✅ **Format Validation** (email, uuid, date-time, uri, int64)
- ✅ **Nullable Fields**
- ✅ **Required/Optional Distinction**

### Response Handling
- ✅ **Multiple Status Codes** (2xx, 4xx, 5xx)
- ✅ **Variant-Based Responses**
- ✅ **Per-Status Headers and Bodies**
- ✅ **Error Response Schemas**

### OpenAPI Metadata
- ✅ **Descriptions** everywhere
- ✅ **Examples** for all types
- ✅ **Deprecated Warnings**
- ✅ **Default Values**
- ✅ **Custom Titles**

### API Documentation
- ✅ **Complete OpenAPI 3.0 Spec Generation**
- ✅ **Multiple Server Definitions** 🆕
- ✅ **Automatic Path Grouping**
- ✅ **Security Scheme Components**

---

## 🚀 Full-Featured Example API

See `examples/FullFeaturedAPI.purs` for the complete code. Here's a summary of the API routes:

### Health Check
```purescript
GET /health
```
- No authentication required
- Returns service health status with enum values
- Correlation ID in response headers

### User Management
```purescript
GET    /users                  -- List with pagination, sorting, role filter
GET    /users/:id              -- Get user with versioned headers
POST   /users                  -- Create with password validation
PUT    /users/:id              -- Update with optimistic locking (If-Match)
DELETE /users/:id              -- Delete with 204 No Content
GET    /users/search?query=... -- Search with required query param
```

### Nested Resources
```purescript
GET /users/:userId/orders  -- List user's orders with status filter
```

---

## 🎨 Feature Demonstrations

### 1. Enum Support 🆕

**Type Definition:**
```purescript
type OrderStatus =
  Description "Current order status"
    ( Example "processing"
        ( Enum ("pending" :~: "processing" :~: "shipped" :~: "delivered" :~: "cancelled" :~: EnumEnd)
            String
        )
    )
```

**Generated OpenAPI:**
```json
{
  "type": "string",
  "description": "Current order status",
  "example": "processing",
  "enum": ["pending", "processing", "shipped", "delivered", "cancelled"]
}
```

**Use Cases:**
- Order statuses
- User roles (admin, user, guest)
- Sort orders (asc, desc)
- Priority levels (low, medium, high, urgent)
- Any constrained set of values

---

### 2. Response Header Metadata 🆕

**Type Definition:**
```purescript
type CorrelationId =
  Description "Request correlation ID for distributed tracing"
    ( Example "550e8400-e29b-41d4-a716-446655440000"
        ( Format "uuid" String )
    )

type RateLimitRemaining =
  Description "Number of requests remaining in current window"
    ( Example "95" Int )

type MyRoute = Route
  GET (Lit "users")
  (Request {})
  ( ok ::
      { body :: User
      , headers ::
          { "x-correlation-id" :: CorrelationId
          , "x-rate-limit-remaining" :: RateLimitRemaining
          }
      }
  )
```

**Generated OpenAPI:**
```json
{
  "200": {
    "description": "Successful response",
    "headers": {
      "x-correlation-id": {
        "description": "Request correlation ID for distributed tracing",
        "example": "550e8400-e29b-41d4-a716-446655440000",
        "schema": {
          "type": "string",
          "format": "uuid"
        }
      },
      "x-rate-limit-remaining": {
        "description": "Number of requests remaining in current window",
        "example": "95",
        "schema": {
          "type": "integer"
        }
      }
    }
  }
}
```

**Use Cases:**
- Distributed tracing (X-Correlation-Id, X-Request-Id)
- Rate limiting (X-RateLimit-Remaining, X-RateLimit-Reset)
- Cache control (Cache-Control, ETag, Last-Modified)
- Content versioning (X-Content-Version)
- Location headers for created resources

---

### 3. Server Definitions 🆕

**Type Definition:**
```purescript
import Yoga.Fastify.Om.Route (buildOpenAPISpec')

apiSpec = buildOpenAPISpec' @(UserRoutes /\ OrderRoutes)
  { title: "My API"
  , version: "1.0.0"
  }
  { servers: Just
      [ { url: "https://api.example.com/v1"
        , description: Just "Production server"
        }
      , { url: "https://staging-api.example.com/v1"
        , description: Just "Staging server"
        }
      , { url: "http://localhost:3000/v1"
        , description: Just "Local development"
        }
      ]
  }
```

**Generated OpenAPI:**
```json
{
  "openapi": "3.0.0",
  "info": {
    "title": "My API",
    "version": "1.0.0"
  },
  "servers": [
    {
      "url": "https://api.example.com/v1",
      "description": "Production server"
    },
    {
      "url": "https://staging-api.example.com/v1",
      "description": "Staging server"
    },
    {
      "url": "http://localhost:3000/v1",
      "description": "Local development"
    }
  ]
}
```

---

### 4. Complex Validation

**Email with Pattern Validation:**
```purescript
type Email =
  Description "User email address"
    ( Example "user@example.com"
        ( Format "email"
            ( Pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
                String
            )
        )
    )
```

**Username with Length Constraints:**
```purescript
type Username =
  Description "Unique username"
    ( Example "john_doe"
        ( MinLength 3
            ( MaxLength 20
                ( Pattern "^[a-zA-Z0-9_]+$"
                    String
                )
            )
        )
    )
```

**Password with Security Requirements:**
```purescript
type Password =
  Description "User password (min 8 chars, must include uppercase, lowercase, digit)"
    ( MinLength 8
        ( MaxLength 72
            ( Pattern "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d).*$"
                String
            )
        )
    )
```

---

### 5. Pagination with Defaults

```purescript
type PageNumber =
  Description "Page number (1-indexed)"
    ( Example "1"
        ( Minimum 1
            ( Default "1" Int )
        )
    )

type PageSize =
  Description "Items per page"
    ( Example "20"
        ( Minimum 1
            ( Maximum 100
                ( Default "20" Int )
            )
        )
    )
```

---

### 6. Bearer Authentication

```purescript
type Authorization =
  Description "JWT bearer token"
    ( Example "Bearer eyJhbGc..."
        BearerToken
    )

type SecureRoute = Route
  GET (Lit "protected")
  (Request { requestHeaders :: { authorization :: Authorization } })
  (ok :: { body :: Data })
```

**Generated Security Scheme:**
```json
{
  "components": {
    "securitySchemes": {
      "bearerAuth": {
        "type": "http",
        "scheme": "bearer",
        "bearerFormat": "JWT"
      }
    }
  }
}
```

---

### 7. Multiple Response Types

```purescript
type GetUserRoute = Route
  GET (Lit "users" /> "id" :> Int)
  (Request { requestHeaders :: { authorization :: BearerToken } })
  ( ok ::
      { body :: User
      , headers :: StandardHeaders
      }
  , notFound ::
      { body :: ErrorResponse
      , headers :: { "x-correlation-id" :: CorrelationId }
      }
  , unauthorized ::
      { body :: ErrorResponse
      , headers :: { "x-correlation-id" :: CorrelationId }
      }
  )
```

**Generated Responses:**
```json
{
  "responses": {
    "200": {
      "description": "Successful response",
      "headers": { ... },
      "content": {
        "application/json": {
          "schema": { "$ref": "#/components/schemas/User" }
        }
      }
    },
    "404": {
      "description": "Successful response",
      "headers": { ... },
      "content": {
        "application/json": {
          "schema": { "$ref": "#/components/schemas/ErrorResponse" }
        }
      }
    },
    "401": { ... }
  }
}
```

---

### 8. Deprecated Fields

```purescript
type LegacyField =
  Description "This field is deprecated, use newField instead"
    ( Deprecated
        ( Example "old-value" String )
    )
```

**Generated Schema:**
```json
{
  "type": "string",
  "description": "This field is deprecated, use newField instead",
  "example": "old-value",
  "deprecated": true
}
```

---

### 9. Nullable Fields

```purescript
type User =
  { id :: Int
  , username :: String
  , bio ::
      Description "User biography (optional)"
        ( Nullable String )
  }
```

**Generated Schema:**
```json
{
  "type": "object",
  "properties": {
    "id": { "type": "integer" },
    "username": { "type": "string" },
    "bio": {
      "type": "string",
      "description": "User biography (optional)",
      "nullable": true
    }
  },
  "required": ["id", "username"]
}
```

---

## 📊 Generated OpenAPI Output

The complete example API generates a fully valid OpenAPI 3.0 specification with:

- **8 routes** demonstrating all features
- **Multiple server environments** (production, staging, dev, local)
- **Bearer authentication** with security schemes
- **Rich parameter validation** with constraints and enums
- **Complex response schemas** with multiple status codes
- **Full header metadata** for both requests and responses
- **Type-safe error responses**

The generated spec is:
- ✅ **Valid OpenAPI 3.0**
- ✅ **Swagger UI compatible**
- ✅ **Postman importable**
- ✅ **Code generator ready**
- ✅ **API Gateway compatible**

---

## 🛠️ Using the Generated Spec

### View in Swagger UI

1. **Online Editor:**
   - Visit https://editor.swagger.io/
   - Import the generated JSON
   - Explore interactive documentation

2. **Local Swagger UI:**
   ```bash
   docker run -p 8080:8080 \
     -v $(pwd):/usr/share/nginx/html/api \
     swaggerapi/swagger-ui
   ```
   Then visit: http://localhost:8080/?url=/api/swagger.json

### Import to Postman

1. Open Postman
2. Import → Upload Files → Select generated JSON
3. All routes automatically created with examples

### Generate Client Code

```bash
# Generate TypeScript client
openapi-generator generate \
  -i swagger.json \
  -g typescript-axios \
  -o ./generated-client

# Generate Python client
openapi-generator generate \
  -i swagger.json \
  -g python \
  -o ./generated-client
```

### Use with API Gateways

The generated spec works with:
- AWS API Gateway
- Kong API Gateway
- Azure API Management
- Google Cloud Endpoints

---

## 🎉 Summary

This library provides **complete OpenAPI 3.0 support** with:

1. **Type-safe route definitions** in PureScript
2. **Automatic OpenAPI generation** from types
3. **Rich metadata** at every level
4. **Full validation** constraints
5. **Multiple environments** (servers)
6. **Security schemes** (bearer tokens)
7. **Complex schemas** (enums, nested objects, arrays)
8. **Response variants** (multiple status codes)

All features are **fully type-checked** at compile time and generate **standards-compliant** OpenAPI 3.0 specifications!

---

## 📖 Additional Resources

- **Full Example Code:** `examples/FullFeaturedAPI.purs`
- **OpenAPI Module:** `src/Yoga/Fastify/Om/Route/OpenAPI.purs`
- **Metadata Types:** `src/Yoga/Fastify/Om/Route/OpenAPIMetadata.purs`
- **Documentation:** `docs/openapi-features.md`
- **Feature Summary:** `NEW_OPENAPI_FEATURES.md`
