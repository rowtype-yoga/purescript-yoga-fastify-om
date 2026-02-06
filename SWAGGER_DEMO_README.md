# 🎯 Swagger/OpenAPI Feature Demo

This directory contains a complete demonstration of all OpenAPI features supported by `purescript-yoga-fastify-om`.

## 📁 Files

1. **`FEATURE_SHOWCASE.md`** - Complete documentation of all features with examples
2. **`OPENAPI_SAMPLE_OUTPUT.json`** - Sample OpenAPI 3.0 spec showing generated output
3. **`examples/FullFeaturedAPI.purs`** - Full PureScript implementation with all features

## ✨ Key Features Demonstrated

### 🆕 Recently Added (Major Features)
- ✅ **Enum/Union Types** - Constrained value sets (statuses, roles, etc.)
- ✅ **Response Header Metadata** - Full metadata support for response headers
- ✅ **Server Definitions** - Multiple environment configurations

### 🔐 Authentication & Security
- Bearer Token (JWT) authentication
- Automatic security scheme generation
- Per-route security requirements

### 📋 Parameters
- Path parameters with validation
- Query parameters (optional & required)
- Pagination & sorting
- Enum constraints
- Default values
- Min/max constraints
- Pattern validation

### 📨 Request/Response
- Multiple response status codes (2xx, 4xx)
- Rich response headers (correlation IDs, rate limiting, ETags)
- Complex request/response bodies
- NoBody responses (204)
- Error responses

### ✅ Validation
- String validation (length, pattern, format)
- Number constraints (min, max)
- Email, UUID, date-time formats
- Required vs optional fields
- Nullable fields
- Deprecated fields

### 🌐 API Documentation
- Complete OpenAPI 3.0 spec
- Multiple servers (prod, staging, dev, local)
- Security components
- Reusable schemas

## 🚀 Quick Start

### View the Sample Spec

**Option 1: Online Swagger Editor**
1. Visit https://editor.swagger.io/
2. File → Import File → `OPENAPI_SAMPLE_OUTPUT.json`
3. Explore the interactive documentation

**Option 2: View Raw JSON**
```bash
cat OPENAPI_SAMPLE_OUTPUT.json | jq .
```

### Understand the Type Definitions

Check out `examples/FullFeaturedAPI.purs` to see how the PureScript types generate the OpenAPI spec.

### Read the Documentation

See `FEATURE_SHOWCASE.md` for:
- Complete feature list with examples
- Type definition patterns
- Generated OpenAPI output
- Usage instructions

## 📊 Example Routes

The demo API includes 8 routes covering all features:

```
GET    /health                    - Health check (enum status)
GET    /users                     - List with pagination & filtering
GET    /users/:id                 - Get user (versioned headers)
POST   /users                     - Create user (validation)
PUT    /users/:id                 - Update user (optimistic locking)
DELETE /users/:id                 - Delete user (204 no content)
GET    /users/:userId/orders      - Nested resource
GET    /users/search?query=...    - Required query parameter
```

## 🎨 Feature Highlights

### Enum Types
```purescript
type OrderStatus =
  Description "Current order status"
    ( Enum ("pending" :~: "processing" :~: "shipped" :~: "delivered" :~: "cancelled" :~: EnumEnd)
        String
    )
```

### Response Headers
```purescript
type CorrelationId =
  Description "Request correlation ID for distributed tracing"
    ( Example "550e8400-e29b-41d4-a716-446655440000"
        ( Format "uuid" String )
    )

type MyRoute = Route GET (Lit "users") (Request {})
  ( ok ::
      { body :: User
      , headers :: { "x-correlation-id" :: CorrelationId }
      }
  )
```

### Multiple Servers
```purescript
apiSpec = buildOpenAPISpec' @Routes
  { title: "My API", version: "1.0.0" }
  { servers: Just
      [ { url: "https://api.example.com", description: Just "Production" }
      , { url: "http://localhost:3000", description: Just "Local dev" }
      ]
  }
```

### Validation
```purescript
type Email =
  Description "User email"
    ( Example "user@example.com"
        ( Format "email"
            ( Pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
                String
            )
        )
    )
```

## 💡 Use Cases

This library is perfect for:

1. **Type-Safe API Development** - Define routes in PureScript, get compile-time guarantees
2. **Automatic Documentation** - Generate OpenAPI specs from types
3. **Client Generation** - Use generated specs with openapi-generator
4. **API Gateway Integration** - Export to AWS Gateway, Kong, etc.
5. **Contract Testing** - Ensure implementation matches spec

## 🛠️ Tools You Can Use

- **Swagger UI** - Interactive API documentation
- **Postman** - Import and test endpoints
- **openapi-generator** - Generate client libraries
- **API Gateways** - AWS, Kong, Azure, Google Cloud
- **Validation Tools** - Spectral, swagger-cli

## 📖 Learn More

- **Main Documentation**: `docs/openapi-features.md`
- **Feature Summary**: `NEW_OPENAPI_FEATURES.md`
- **Schema Introspection**: `SCHEMA_INTROSPECTION.md`
- **Example Code**: `examples/FullFeaturedAPI.purs`

## ✅ Status

All features are **fully implemented** and **production ready**:
- ✅ Type-safe route definitions
- ✅ Complete OpenAPI 3.0 generation
- ✅ All metadata wrappers working
- ✅ Full validation support
- ✅ Multiple response types
- ✅ Security schemes
- ✅ Server configurations
- ✅ Response header metadata
- ✅ Enum types

---

**Built with** ❤️ **using PureScript & Type-Level Programming**
