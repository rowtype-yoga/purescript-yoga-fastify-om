module Test.ServerIntegration.Spec where

import Prelude

import Control.Monad.Reader (ask)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Om.Route (GET, POST, Route, Request, Handler, JSON, handleRoute, handle, respond, reject)

--------------------------------------------------------------------------------
-- Test Routes (mirrors examples/Server.purs structure)
--------------------------------------------------------------------------------

type HealthRoute = Route GET "health"
  (Request {})
  ( ok :: { body :: { status :: String } }
  , serviceUnavailable :: { body :: { error :: String } }
  )

healthHandler :: Handler HealthRoute
healthHandler = handle do
  respond { ok: { status: "healthy" } }

type EchoRoute = Route POST "echo"
  (Request { body :: JSON { message :: String } })
  ( ok :: { body :: { echo :: String } }
  , badRequest :: { body :: { error :: String } }
  )

echoHandler :: Handler EchoRoute
echoHandler = handle do
  ctx <- ask
  let
    message = ctx.body.message
  when (String.null message) do
    reject { badRequest: { error: "Message cannot be empty" } }
  respond { ok: { echo: message } }

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testServerCompilation :: Effect ViTest
testServerCompilation = describe "Server Integration" do
  _ <- test "compiles health route handler" do
    fastify <- liftEffect $ F.fastify {}
    liftEffect $ handleRoute healthHandler fastify
    expectToBe true true

  _ <- test "compiles echo route handler" do
    fastify <- liftEffect $ F.fastify {}
    liftEffect $ handleRoute echoHandler fastify
    expectToBe true true

  test "can register multiple routes on same server" do
    fastify <- liftEffect $ F.fastify {}
    liftEffect $ handleRoute healthHandler fastify
    liftEffect $ handleRoute echoHandler fastify
    expectToBe true true
