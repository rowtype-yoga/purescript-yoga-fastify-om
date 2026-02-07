module Test.Multithreading where

import Prelude

import Control.Monad.Reader.Trans (ask)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Node.Path as Path
import Node.Process as Process
import Node.WorkerBees as WB
import Node.WorkerBees.Aff.Pool as Pool

import Yoga.Fastify.Fastify (Host(..), Port(..))
import Yoga.Fastify.Fastify as F
import Yoga.HTTP.API.Path (type (/), type (:))
import Yoga.Fastify.Om.Route (GET, POST, Route, Request, Handler, JSON, handleRoute, handle, respond, reject)
import Yoga.Om as Om
import Yoga.Om.WorkerBees (WorkerPool)

-- Types

type FibInput = { n :: Int }
type FibOutput = { result :: Int, thread :: Int }

type HashInput = { text :: String, iterations :: Int }
type HashOutput = { hash :: String, length :: Int }

-- Routes

type HealthRoute = Route GET
  "health"
  (Request {})
  ( ok :: { body :: { status :: String } }
  )

type FibRoute = Route GET
  ("fib" / "n" : Int)
  (Request {})
  ( ok :: { body :: { n :: Int, result :: Int } }
  , badRequest :: { body :: { error :: String } }
  )

type FibBatchRoute = Route POST
  ("fib" / "batch")
  (Request { body :: JSON { numbers :: Array Int } })
  ( ok :: { body :: { results :: Array { n :: Int, result :: Int } } }
  , badRequest :: { body :: { error :: String } }
  )

type HashRoute = Route POST
  "hash"
  (Request { body :: JSON { text :: String, iterations :: Int } })
  ( ok :: { body :: { hash :: String, length :: Int } }
  , badRequest :: { body :: { error :: String } }
  )

type HashBatchRoute = Route POST
  ("hash" / "batch")
  (Request { body :: JSON { inputs :: Array { text :: String, iterations :: Int } } })
  ( ok :: { body :: { results :: Array { hash :: String, length :: Int } } }
  , badRequest :: { body :: { error :: String } }
  )

type API =
  HealthRoute
    /\ FibRoute
    /\ FibBatchRoute
    /\ HashRoute
    /\ HashBatchRoute

-- Handlers

healthHandler :: Handler HealthRoute
healthHandler = handle do
  respond { ok: { status: "ok" } }

fibHandler :: WorkerPool FibInput FibOutput -> Handler FibRoute
fibHandler pool = handle do
  { path } <- ask
  let n = path.n
  when (n < 0) do
    reject { badRequest: { error: "n must be non-negative" } }
  when (n > 40) do
    reject { badRequest: { error: "n must be <= 40" } }
  result <- Om.fromAff $ Pool.invoke pool { n }
  respond { ok: { n, result: result.result } }

fibBatchHandler :: WorkerPool FibInput FibOutput -> Handler FibBatchRoute
fibBatchHandler pool = handle do
  { body } <- ask
  let numbers = body.numbers
  when (Array.any (\n -> n < 0) numbers) do
    reject { badRequest: { error: "all numbers must be non-negative" } }
  when (Array.any (\n -> n > 40) numbers) do
    reject { badRequest: { error: "all numbers must be <= 40" } }
  when (Array.length numbers > 100) do
    reject { badRequest: { error: "maximum 100 numbers per batch" } }
  let inputs = numbers <#> \n -> { n }
  outputs <- Om.fromAff $ parTraverse (Pool.invoke pool) inputs
  let results = Array.zipWith (\n output -> { n, result: output.result }) numbers outputs
  respond { ok: { results } }

hashHandler :: WorkerPool HashInput HashOutput -> Handler HashRoute
hashHandler pool = handle do
  { body } <- ask
  when (body.iterations < 1) do
    reject { badRequest: { error: "iterations must be >= 1" } }
  when (body.iterations > 1000) do
    reject { badRequest: { error: "iterations must be <= 1000" } }
  result <- Om.fromAff $ Pool.invoke pool { text: body.text, iterations: body.iterations }
  respond { ok: result }

hashBatchHandler :: WorkerPool HashInput HashOutput -> Handler HashBatchRoute
hashBatchHandler pool = handle do
  { body } <- ask
  let inputs = body.inputs
  when (Array.any (\inp -> inp.iterations < 1) inputs) do
    reject { badRequest: { error: "all iterations must be >= 1" } }
  when (Array.any (\inp -> inp.iterations > 1000) inputs) do
    reject { badRequest: { error: "all iterations must be <= 1000" } }
  when (Array.length inputs > 50) do
    reject { badRequest: { error: "maximum 50 hashes per batch" } }
  results <- Om.fromAff $ parTraverse (Pool.invoke pool) inputs
  respond { ok: { results } }

-- Server Setup

startServer :: Aff Unit
startServer = do
  cwd <- liftEffect Process.cwd
  let
    fibWorkerPath = Path.concat [ cwd, "dist", "workers", "FibonacciWorker.js" ]
    hashWorkerPath = Path.concat [ cwd, "dist", "workers", "HashWorker.js" ]

  liftEffect $ Console.log "Starting worker pools..."

  let fibWorker = WB.unsafeWorkerFromPath fibWorkerPath
  let hashWorker = WB.unsafeWorkerFromPath hashWorkerPath
  fibPool <- Pool.make fibWorker unit 4
  hashPool <- Pool.make hashWorker unit 4

  liftEffect $ Console.log "Fibonacci worker pool started (4 workers)"
  liftEffect $ Console.log "Hash worker pool started (4 workers)"

  app <- liftEffect $ F.fastify {}

  liftEffect $ handleRoute healthHandler app
  liftEffect $ handleRoute (fibHandler fibPool) app
  liftEffect $ handleRoute (fibBatchHandler fibPool) app
  liftEffect $ handleRoute (hashHandler hashPool) app
  liftEffect $ handleRoute (hashBatchHandler hashPool) app

  let port = 3000
  liftEffect $ Console.log $ "Server starting on port " <> show port
  liftEffect $ Console.log $ "  curl http://localhost:" <> show port <> "/health"
  liftEffect $ Console.log $ "  curl http://localhost:" <> show port <> "/fib/30"

  address <- F.listen { port: Port port, host: Host "0.0.0.0" } app
  liftEffect $ Console.log $ "Listening on " <> address

main :: Effect Unit
main = launchAff_ startServer
