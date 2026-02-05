module Yoga.Fastify.Om.Route.Encoding
  ( JSON
  , NoBody
  ) where

-- | JSON-encoded request body
-- |
-- | Example:
-- |   { requestBody :: JSON User }
data JSON :: Type -> Type
data JSON a

-- | No request body (for GET, DELETE, etc.)
-- |
-- | Example:
-- |   { requestBody :: NoBody }
data NoBody
