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

## Documentation by Example

The test directory contains complete, runnable examples that serve as documentation:

### Examples (Complete Servers)

1. [`test/01-BasicAPI.purs`](test/01-BasicAPI.purs) - REST API with authentication, validation, and OpenAPI generation
2. [`test/02-Multithreading.purs`](test/02-Multithreading.purs) - CPU-intensive work with worker threads
3. [`test/03-SharedState.purs`](test/03-SharedState.purs) - Atomic shared state across workers

Each example is a complete, working server demonstrating real-world patterns.

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
