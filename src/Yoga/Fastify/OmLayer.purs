module Yoga.Fastify.OmLayer
  ( FastifyConfig
  , FastifyL
  , fastifyLayer
  , fastifyLayer'
  ) where

import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Yoga.Fastify.Fastify (Fastify, RouteHandler)
import Yoga.Fastify.Fastify as F
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer)

-- | Fastify server configuration
type FastifyConfig =
  { port :: F.Port
  , host :: F.Host
  }

-- | Row type for Fastify service
type FastifyL r = (fastify :: Fastify | r)

-- | Create a Fastify layer that provides Fastify server as a service
-- | Requires FastifyConfig in context
-- | Note: This creates the server but doesn't start listening - use fastifyServerLayer for that
fastifyLayer :: forall r. OmLayer (fastifyConfig :: FastifyConfig | r) (FastifyL ()) ()
fastifyLayer = makeLayer do
  { fastifyConfig } <- Om.ask
  app <- liftEffect $ F.fastify { logger: false }
  liftEffect $ Console.log $
    "🚀 Fastify server created (ready to listen on " <> show fastifyConfig.port <> ")"
  pure { fastify: app }

-- | Create a Fastify layer with inline config
-- | Useful when you don't need config from context
fastifyLayer'
  :: forall r
   . FastifyConfig
  -> OmLayer r (FastifyL ()) ()
fastifyLayer' config = makeLayer do
  app <- liftEffect $ F.fastify { logger: false }
  liftEffect $ Console.log $
    "🚀 Fastify server created (ready to listen on " <> show config.port <> ")"
  pure { fastify: app }

-- | Create a layer that starts the Fastify server listening
-- | Requires both FastifyConfig and Fastify in context
type FastifyServerL r = (fastifyServer :: String | r)

fastifyServerLayer
  :: forall r
   . OmLayer (fastifyConfig :: FastifyConfig, fastify :: Fastify | r) (FastifyServerL ()) ()
fastifyServerLayer = makeLayer do
  { fastifyConfig, fastify } <- Om.ask
  address <- liftAff $ F.listen
    { port: fastifyConfig.port
    , host: fastifyConfig.host
    }
    fastify
  liftEffect $ Console.log $ "🌐 Fastify server listening at " <> address
  pure { fastifyServer: address }
