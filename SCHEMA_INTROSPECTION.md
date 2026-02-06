# Request/Response Body Schema Introspection

## Overview

The OpenAPI generation now includes **full type introspection** for request and response bodies. Previously, bodies were rendered as generic `{ type: "object" }` with no property information. Now, the system walks the type structure and generates complete OpenAPI schemas with property-level details.

## What Was Implemented

### 1. Core Type Introspection (`RenderJSONSchema` typeclass)

A new typeclass that converts PureScript types into OpenAPI JSON schemas:

```purescript
class RenderJSONSchema ty where
  renderJSONSchema :: Proxy ty -> Foreign
```

### 2. Supported Types

#### Primitives
- `String` → `{ type: "string" }`
- `Int` → `{ type: "integer" }`
- `Number` → `{ type: "number" }`
- `Boolean` → `{ type: "boolean" }`
- `Unit` → `{ type: "null" }`

#### Complex Types
- **Records** → `{ type: "object", properties: {...}, required: [...] }`
  - Uses RowList traversal (inspired by Yoga.JSON)
  - Automatically detects required vs optional fields
  - Supports nested records
- **Arrays** → `{ type: "array", items: {...} }`
  - Recursively introspects item type
- **Maybe** → Adds `nullable: true` to the inner type's schema
- **JSON wrapper** → Unwraps and introspects the inner type

#### Metadata Wrappers (Partial Support)
- `Description` → Adds `description` field
- `Example` → Adds `example` field
- `Format` → Adds `format` field (e.g., "email", "uuid")
- `Nullable` → Adds `nullable: true`
- `Deprecated` → Adds `deprecated: true`

Note: Numeric constraint wrappers (Minimum, Maximum, etc.) are not yet supported in body schemas due to complexity.

### 3. Integration Points

#### Request Bodies
The `RenderRequestBodySchema` typeclass now uses `RenderJSONSchema`:

```purescript
-- Before:
{ schema: { type: "object" } }

-- After:
{ schema: {
    type: "object",
    properties: {
      id: { type: "integer" },
      name: { type: "string" },
      email: { type: "string", format: "email" }
    },
    required: ["id", "name", "email"]
  }
}
```

#### Response Bodies
The `RenderVariantResponseSchemaRL` typeclass extracts the body type from each variant case and introspects it:

```purescript
( ok :: { body :: JSON User }
, notFound :: { body :: JSON { error :: String } }
)
```

Both `User` and `{ error :: String }` are fully introspected.

## Example

```purescript
type User =
  { id :: Int
  , name :: String
  , email :: Format "email" String
  , age :: Maybe Int
  , tags :: Array String
  }

type GetUserRoute = Route
  GET
  (Lit "users" / "userId" :> Int)
  (Request {})
  ( ok :: { body :: JSON User }
  , notFound :: { body :: JSON { error :: String } }
  )
```

### Generated OpenAPI Schema (excerpt)

```json
{
  "paths": {
    "/users/{userId}": {
      "get": {
        "responses": {
          "200": {
            "content": {
              "application/json": {
                "schema": {
                  "type": "object",
                  "properties": {
                    "id": { "type": "integer" },
                    "name": { "type": "string" },
                    "email": {
                      "type": "string",
                      "format": "email"
                    },
                    "age": {
                      "type": "integer",
                      "nullable": true
                    },
                    "tags": {
                      "type": "array",
                      "items": { "type": "string" }
                    }
                  },
                  "required": ["id", "name", "email", "tags"]
                }
              }
            }
          },
          "404": {
            "content": {
              "application/json": {
                "schema": {
                  "type": "object",
                  "properties": {
                    "error": { "type": "string" }
                  },
                  "required": ["error"]
                }
              }
            }
          }
        }
      }
    }
  }
}
```

## Architecture

The implementation follows the same pattern as Yoga.JSON:

1. **RowList Traversal**: Records are converted to RowLists and traversed recursively
2. **Functional Dependencies**: `RenderRecordSchemaRL rl row | rl -> row` ensures type safety
3. **Proxy-based Resolution**: Type-level computation uses `Proxy` to pass types
4. **Foreign Objects**: Schemas are built as `Foreign.Object Foreign` for flexibility

### Key Files

- `src/Yoga/Fastify/Om/Route/OpenAPI.purs` - Core introspection implementation
  - `RenderJSONSchema` typeclass (lines 332-502)
  - `RenderRecordSchemaRL` helper class for record traversal
- `test/RouteTest.Spec.purs` - Tests updated to verify schema structure

## Benefits

1. **Richer Documentation**: Generated specs now show exact field types and structures
2. **Better Client Generation**: Tools like openapi-generator can create properly-typed clients
3. **Validation Support**: OpenAPI validators can check request/response conformance
4. **IDE Support**: Swagger UI shows detailed schema information
5. **Type Safety**: Schema generation is driven entirely by compile-time types

## Limitations & Future Work

### Current Limitations
1. **Metadata Wrappers**: Numeric constraints (Minimum, Maximum, MinLength, MaxLength, Pattern) not yet supported in body schemas
2. **Reusable Schemas**: No `$ref` support - schemas are inlined everywhere
3. **Custom Types**: No support for newtypes, sum types (variants in bodies), or custom serialization
4. **One Content Type**: Only `application/json` is supported

### Future Enhancements
1. **Component Schemas**: Add `components/schemas` section with `$ref` deduplication
2. **Full Metadata Support**: Complete all constraint wrappers in body schemas
3. **Enum Support**: String literal unions as OpenAPI enums
4. **Content Type Flexibility**: Support form-data, binary, etc.
5. **Custom Schema Annotations**: Type-level annotations for examples, defaults, etc.

## Impact

This closes the **biggest OpenAPI gap** - request/response body introspection. The other gaps (security schemes, operation metadata, etc.) are important but less critical for basic API documentation and client generation.

Before this change:
- Bodies: `{ type: "object" }` (useless for documentation)

After this change:
- Bodies: Full property-level schemas with types, constraints, and metadata

This is a **major improvement** in the quality of generated OpenAPI specs.
