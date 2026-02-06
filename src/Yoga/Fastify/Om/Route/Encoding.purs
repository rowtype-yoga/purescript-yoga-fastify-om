module Yoga.Fastify.Om.Route.Encoding
  ( JSON
  , FormData
  , NoBody
  ) where

-- | JSON-encoded request body
-- |
-- | Example:
-- |   Request { body :: JSON User }
data JSON :: Type -> Type
data JSON a

-- | Form data encoded request body (application/x-www-form-urlencoded or multipart/form-data)
-- |
-- | Example:
-- |   Request { body :: FormData { username :: String, password :: String } }
data FormData :: Type -> Type
data FormData a

-- | No request body (for GET, DELETE, etc.)
-- |
-- | Example:
-- |   Request {}  -- NoBody is the default when body is omitted
data NoBody
