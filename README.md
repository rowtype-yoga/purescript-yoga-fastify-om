# purescript-yoga-fastify-om

Type-safe HTTP routing for Fastify with Om effect management.

## Features

- Type-level route definitions with path parameters, query params, headers, and request bodies
- Variant-based response handling with multiple status codes
- Automatic OpenAPI specification generation
- Built-in validation (pattern, min/max length, min/max value)
- Om-based effect management with structured error handling
- Multi-threading support via WorkerBees
- Shared memory primitives (SharedInt, SharedState)

## Learning by Example

### Complete Working Examples

Start with these complete, runnable servers:

- [`test/07-Integration-BasicAPI.purs`](test/07-Integration-BasicAPI.purs) - REST API with auth, validation, and OpenAPI
- [`test/08-Integration-Multithreading.purs`](test/08-Integration-Multithreading.purs) - CPU-intensive work with worker threads
- [`test/09-Integration-SharedState.purs`](test/09-Integration-SharedState.purs) - Shared state across workers

### Feature Tests

See [`test/Test.Operators.purs`](test/Test.Operators.purs) for detailed tests of individual features:

1. **Path Parsing** - Captures, segments, type-safe parameter extraction
2. **Query Parameters** - Optional and required params
3. **Headers** - Type-safe headers including bearer token auth
4. **Request Bodies** - JSON, FormData, streaming
5. **Responses** - Variant-based responses with multiple status codes
6. **Validation** - Min/max length, min/max value, pattern matching

## Quick Start

```purescript
import Yoga.Fastify.Om.Route (GET, Route, Request, Handler, handle, respond, handleRoute)

-- Define a route
type HealthRoute = Route GET "health"
  (Request {})
  ( ok :: { body :: { status :: String } }
  )

-- Implement the handler
healthHandler :: Handler HealthRoute
healthHandler = handle do
  respond { ok: { status: "healthy" } }

-- Register with Fastify
main = do
  fastify <- F.fastify {}
  handleRoute healthHandler fastify
  F.listen { port: Port 3000, host: Host "0.0.0.0" } fastify
```

## Installation

```bash
spago install yoga-fastify-om
```

## License

MIT
