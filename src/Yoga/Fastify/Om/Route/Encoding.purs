module Yoga.Fastify.Om.Route.Encoding
  ( JSON
  , NoBody
  ) where

-- | JSON-encoded request body
-- |
-- | Example:
-- |   Request { body :: JSON User }
data JSON :: Type -> Type
data JSON a

-- | No request body (for GET, DELETE, etc.)
-- |
-- | Example:
-- |   Request {}  -- NoBody is the default when body is omitted
data NoBody
