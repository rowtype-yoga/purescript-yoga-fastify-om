module Yoga.Fastify.Om.API
  ( registerAPI
  , registerAPILayer
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object as FObject
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Fastify.Fastify (Fastify)
import Yoga.Om (Om)
import Yoga.Om.Layer (OmLayer, makeLayer)

-- | Register all routes from an API record to a Fastify instance
-- |
-- | Example:
-- | ```purescript
-- | type API = { health :: HealthRoute, user :: UserRoute }
-- | handlers = { health: healthHandler, user: userHandler }
-- |
-- | fastify <- F.fastify {}
-- | registerAPI handlers fastify
-- | ```
registerAPI
  :: forall handlers
   . Record handlers
  -> Fastify
  -> Effect Unit
registerAPI handlers fastify = do
  let
    handlersObj = unsafeCoerce handlers :: FObject.Object (Fastify -> Effect Unit)
    handleRoute' = \handler -> handler fastify
  traverse_ handleRoute' handlersObj

-- | OmLayer that registers API routes, requiring fastify in context
-- |
-- | Example:
-- | ```purescript
-- | main = do
-- |   fastify <- F.fastify {}
-- |   runOm { fastify } {}
-- |     $ void $ runLayer { fastify }
-- |     $ registerAPILayer handlers
-- | ```
registerAPILayer
  :: forall handlers ctx err
   . Record handlers
  -> OmLayer (fastify :: Fastify | ctx) () err
registerAPILayer handlers = makeLayer do
  { fastify } <- ask
  let
    handlersObj = unsafeCoerce handlers :: FObject.Object (Fastify -> Effect Unit)
    handleRoute' = \handler -> handler fastify
  traverse_ handleRoute' handlersObj # liftEffect
  pure {}
