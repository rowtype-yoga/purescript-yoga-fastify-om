module Yoga.HTTP.API.Route.HeaderValue
  ( class HeaderValue
  , parseHeader
  , printHeader
  , class HeaderValueType
  , headerValueType
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- HeaderValue Typeclass
--------------------------------------------------------------------------------

-- | Typeclass for values that can be parsed from and printed to HTTP header strings
-- |
-- | This mirrors the `ParseParam` pattern from path parsing and allows headers
-- | to be typed beyond just strings.
-- |
-- | Returns Either String for detailed error messages (the string is the reason, not the header name).
-- |
-- | Examples:
-- |   parseHeader "42" :: Either String Int = Right 42
-- |   parseHeader "bad" :: Either String Int = Left "not a valid integer"
-- |   printHeader 42 = "42"
-- |   parseHeader "hello" :: Either String String = Right "hello"
class HeaderValue a where
  parseHeader :: String -> Either String a
  printHeader :: a -> String

-- | String headers are pass-through (identity)
instance HeaderValue String where
  parseHeader = Right
  printHeader = identity

-- | Integer headers are parsed from strings
instance HeaderValue Int where
  parseHeader s = case Int.fromString s of
    Just i -> Right i
    Nothing -> Left $ "not a valid integer (got: " <> s <> ")"
  printHeader = show

-- | Optional headers (Maybe a) where a has HeaderValue
-- | This instance allows headers to be optional
-- | Note: parseHeader always succeeds for Maybe types (returns Just or Nothing)
instance HeaderValue a => HeaderValue (Maybe a) where
  parseHeader s = Right
    ( case parseHeader s of
        Right val -> Just val
        Left _ -> Nothing
    )
  printHeader Nothing = ""
  printHeader (Just a) = printHeader a

--------------------------------------------------------------------------------
-- HeaderValueType Typeclass (for OpenAPI)
--------------------------------------------------------------------------------

-- | Get OpenAPI type string for a HeaderValue type
class HeaderValueType (ty :: Type) where
  headerValueType :: Proxy ty -> String

instance HeaderValueType String where
  headerValueType _ = "string"

else instance HeaderValueType Int where
  headerValueType _ = "integer"

else instance HeaderValueType a => HeaderValueType (Maybe a) where
  headerValueType _ = headerValueType (Proxy :: Proxy a)
