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

See [`test/Test.Operators.purs`](test/Test.Operators.purs) for a comprehensive test suite that progresses from simple to complex:

1. **Basic Building Blocks** - Path rendering, segments, captures
2. **Path Parsing** - Type-safe path parameter extraction
3. **Query Parameters** - Optional and required query params
4. **Headers** - Type-safe header parsing including bearer tokens
5. **Request Bodies** - JSON, FormData, streaming bodies
6. **Responses** - Variant-based responses with multiple status codes
7. **Validation** - Built-in validators for strings and numbers
8. **OpenAPI Generation** - Automatic API documentation
9. **Full Server Integration** - Complete working examples

For full working servers, see:
- [`test/Server.Example.purs`](test/Server.Example.purs) - Standard REST API
- [`test/MultiThreadedServer.Example.purs`](test/MultiThreadedServer.Example.purs) - CPU-intensive work with worker threads
- [`test/SharedCounterServer.Example.purs`](test/SharedCounterServer.Example.purs) - Shared state across workers

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
