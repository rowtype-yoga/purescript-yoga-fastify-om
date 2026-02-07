module Yoga.Fastify.Om.RequestBody
  ( RequestBody(..)
  , toStrom
  ) where

import Prelude

import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Yoga.Om.Strom (Strom)
import Yoga.Om.Strom as Strom
import Yoga.Om.Strom.WebStream (ReadableStream)
import Yoga.Om.Strom.WebStream as WebStream

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
  | BytesBody Buffer -- Raw bytes (Node.js Buffer)
  | StreamBody (ReadableStream Buffer) -- Streaming bytes (Web Streams API)

-- Note: No Eq instance because Buffer and ReadableStream don't implement Eq

instance Show a => Show (RequestBody a) where
  show (JSONBody a) = "JSONBody (" <> show a <> ")"
  show NoBody = "NoBody"
  show (FormData obj) = "FormData (" <> show (Object.keys obj) <> ")"
  show (TextBody s) = "TextBody \"" <> s <> "\""
  show (BytesBody _) = "BytesBody <buffer>"
  show (StreamBody _) = "StreamBody <stream>"

-- | Convert a RequestBody to a Strom of Buffers for streaming processing.
-- |
-- | - BytesBody: emits the buffer as a single chunk
-- | - StreamBody: streams chunks from the ReadableStream
-- | - TextBody: converts the string to a Buffer and emits it
-- | - JSONBody/FormData/NoBody: emits nothing (empty stream)
-- |
-- | Example:
-- |   handler { request } = do
-- |     let stream = toStrom request.body
-- |     Strom.runFold (\acc chunk -> acc + Buffer.size chunk) 0 stream
toStrom :: forall a ctx err. RequestBody a -> Strom ctx err Buffer
toStrom = case _ of
  BytesBody buf -> Strom.succeed buf
  StreamBody stream -> WebStream.fromReadableStream stream
  TextBody str -> do
    let buf = Buffer.fromString str UTF8
    Strom.fromOm (liftEffect buf <#> Strom.succeed) # Strom.bindStrom identity
  JSONBody _ -> Strom.empty
  FormData _ -> Strom.empty
  NoBody -> Strom.empty
