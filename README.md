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
-- PUT /users/:name
type PutUser = Route PUT
  ("users" / "name" : String)
  { body :: JSON { email :: String } }
  ( ok :: { body :: User }
  , created :: { body :: User }
  )

-- GET /users?limit=10
type ListUsers = Route GET
  ("users" :? { limit :: Int })
  {}
  ( ok :: { body :: Array User } )

type API =
  { putUser :: PutUser
  , listUsers :: ListUsers
  }

-- Handlers declare their dependencies via the Om context
putUserHandler :: Handler PutUser (userRepo :: UserRepo)
putUserHandler = handle do
  { path, body, userRepo } <- ask
  existing <- userRepo.findByName path.name # liftAff
  case existing of
    Just user -> respond @"ok" user
    Nothing -> do
      user <- userRepo.create path.name body.email # liftAff
      respond @"created" user

listUsersHandler :: Handler ListUsers (userRepo :: UserRepo)
listUsersHandler = handle do
  { query, userRepo } <- ask
  users <- userRepo.list query.limit # liftAff
  respond @"ok" users

-- Dependencies are provided via the layer's context
apiLayer :: OmLayer (fastify :: Fastify, userRepo :: UserRepo) () ()
apiLayer = registerAPILayer @API
  { putUser: putUserHandler
  , listUsers: listUsersHandler
  }
```

## Installation

```bash
spago install yoga-fastify-om
```

## License

MIT
