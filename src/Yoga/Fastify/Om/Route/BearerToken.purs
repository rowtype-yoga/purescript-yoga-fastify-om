module Yoga.Fastify.Om.Route.BearerToken
  ( BearerToken(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Yoga.Fastify.Om.Route.HeaderValue (class HeaderValue, class HeaderValueType)

--------------------------------------------------------------------------------
-- BearerToken
--------------------------------------------------------------------------------

-- | Bearer token newtype that validates the "Bearer " prefix
-- |
-- | Example:
-- |   parseHeader "Bearer abc123" :: Either String BearerToken
-- |     = Right (BearerToken "abc123")
-- |   parseHeader "abc123" :: Either String BearerToken
-- |     = Left "missing 'Bearer ' prefix"
newtype BearerToken = BearerToken String

derive instance Eq BearerToken
derive newtype instance Show BearerToken
derive instance Newtype BearerToken _

instance HeaderValue BearerToken where
  parseHeader s =
    if String.take 7 s == "Bearer " then Right $ BearerToken $ String.drop 7 s
    else Left "missing 'Bearer ' prefix"

  printHeader (BearerToken token) = "Bearer " <> token

instance HeaderValueType BearerToken where
  headerValueType _ = "string"
