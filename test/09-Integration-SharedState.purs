module Test.Integration.SharedState where

import Prelude

import Control.Monad.Reader.Trans (ask)
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
import Yoga.Fastify.Om.Route (GET, Route, Request, Handler, handleRoute, handle, respond, reject)
import Yoga.Om as Om
import Yoga.Om.WorkerBees.SharedInt as SharedInt

-- Types

type CounterInput = { n :: Int }
type CounterOutput = { result :: Int, count :: Int }

-- Routes

type FibRoute = Route GET
  ("fib" / "n" : Int)
  (Request {})
  ( ok :: { body :: { n :: Int, result :: Int, totalProcessed :: Int } }
  , badRequest :: { body :: { error :: String } }
  )

type CountRoute = Route GET
  "count"
  (Request {})
  ( ok :: { body :: { totalProcessed :: Int } }
  )

type API = FibRoute /\ CountRoute

-- Handlers

fibHandler :: Pool.WorkerPool CounterInput CounterOutput -> Handler FibRoute
fibHandler pool = handle do
  { path } <- ask
  let n = path.n
  when (n < 0) do
    reject { badRequest: { error: "n must be non-negative" } }
  when (n > 40) do
    reject { badRequest: { error: "n must be <= 40" } }
  output <- Om.fromAff $ Pool.invoke pool { n }
  respond { ok: { n, result: output.result, totalProcessed: output.count } }

countHandler :: SharedInt.SharedInt -> Handler CountRoute
countHandler counter = handle do
  totalProcessed <- Om.fromAff $ liftEffect $ SharedInt.read counter
  respond { ok: { totalProcessed } }

-- Server

startServer :: Aff Unit
startServer = do
  cwd <- liftEffect Process.cwd
  let workerPath = Path.concat [ cwd, "dist", "workers", "CounterWorker.js" ]

  -- Create shared counter visible to all workers
  counter <- liftEffect $ SharedInt.new 0
  liftEffect $ Console.log "Created shared counter (initial: 0)"

  -- Create worker pool, passing the shared counter as workerData
  let worker = WB.unsafeWorkerFromPath workerPath
  pool <- Pool.make worker (SharedInt.toSendable counter) 4
  liftEffect $ Console.log "Worker pool started (4 workers with shared counter)"

  app <- liftEffect $ F.fastify {}
  liftEffect $ handleRoute (fibHandler pool) app
  liftEffect $ handleRoute (countHandler counter) app

  let port = 3001
  liftEffect $ Console.log $ "  curl http://localhost:" <> show port <> "/fib/30"
  liftEffect $ Console.log $ "  curl http://localhost:" <> show port <> "/count"

  address <- F.listen { port: Port port, host: Host "0.0.0.0" } app
  liftEffect $ Console.log $ "Listening on " <> address

main :: Effect Unit
main = launchAff_ startServer
