module Yoga.Fastify.Om.RequestBody
  ( RequestBody(..)
  ) where

import Prelude

import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | Sum type representing different request body formats
-- |
-- | Handlers receive wrapped RequestBody values and pattern match on them.
-- |
-- | Example usage:
-- |   type MyRequest =
-- |     { body :: JSONBody CreateUserRequest
-- |     }
-- |
-- |   handler { request } = case request.body of
-- |     JSONBody user -> createUser user
-- |     NoBody -> error "No body provided"
data RequestBody a
  = JSONBody a -- JSON parsed via yoga-json ReadForeign
  | NoBody -- No body (for GET, DELETE, etc.)
  | FormData (Object String) -- URL-encoded form data (key-value pairs)
  | TextBody String -- Plain text body
  | BytesBody Foreign -- Raw bytes (Buffer/ArrayBuffer)

-- Note: No Eq instance because BytesBody contains Foreign which doesn't implement Eq

instance Show a => Show (RequestBody a) where
  show (JSONBody a) = "JSONBody (" <> show a <> ")"
  show NoBody = "NoBody"
  show (FormData obj) = "FormData (" <> show (Object.keys obj) <> ")"
  show (TextBody s) = "TextBody \"" <> s <> "\""
  show (BytesBody _) = "BytesBody <buffer>"
