module Yoga.HTTP.API.Route.Auth
  ( BearerToken(..)
  , BasicAuth(..)
  , ApiKeyHeader(..)
  , ApiKeyCookie(..)
  , DigestAuth(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Yoga.HTTP.API.Route.HeaderValue (class HeaderValue, class HeaderValueType)

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

--------------------------------------------------------------------------------
-- BasicAuth
--------------------------------------------------------------------------------

-- | Basic authentication header value (Base64-encoded credentials)
-- |
-- | Example:
-- |   parseHeader "Basic dXNlcjpwYXNz" :: Either String BasicAuth
-- |     = Right (BasicAuth "dXNlcjpwYXNz")
-- |
-- | The raw Base64-encoded credentials are preserved for server-side verification
newtype BasicAuth = BasicAuth String

derive instance Eq BasicAuth
derive newtype instance Show BasicAuth
derive instance Newtype BasicAuth _

instance HeaderValue BasicAuth where
  parseHeader s =
    if String.take 6 s == "Basic " then Right $ BasicAuth $ String.drop 6 s
    else Left "missing 'Basic ' prefix"

  printHeader (BasicAuth credentials) = "Basic " <> credentials

instance HeaderValueType BasicAuth where
  headerValueType _ = "string"

--------------------------------------------------------------------------------
-- ApiKeyHeader
--------------------------------------------------------------------------------

-- | API Key passed in a custom header
-- |
-- | Example:
-- |   parseHeader "abc123" :: Either String ApiKeyHeader
-- |     = Right (ApiKeyHeader "abc123")
newtype ApiKeyHeader = ApiKeyHeader String

derive instance Eq ApiKeyHeader
derive newtype instance Show ApiKeyHeader
derive instance Newtype ApiKeyHeader _

instance HeaderValue ApiKeyHeader where
  parseHeader = Right <<< ApiKeyHeader
  printHeader (ApiKeyHeader key) = key

instance HeaderValueType ApiKeyHeader where
  headerValueType _ = "string"

--------------------------------------------------------------------------------
-- ApiKeyCookie
--------------------------------------------------------------------------------

-- | API Key passed in a cookie (for authentication purposes)
-- | This is a marker type that indicates the cookie should be treated as
-- | an authentication mechanism in OpenAPI (securitySchemes) rather than
-- | a regular cookie parameter.
-- |
-- | Example:
-- |   Request { cookies :: { sessionId :: ApiKeyCookie } }
newtype ApiKeyCookie = ApiKeyCookie String

derive instance Eq ApiKeyCookie
derive newtype instance Show ApiKeyCookie
derive instance Newtype ApiKeyCookie _

instance HeaderValue ApiKeyCookie where
  parseHeader = Right <<< ApiKeyCookie
  printHeader (ApiKeyCookie key) = key

instance HeaderValueType ApiKeyCookie where
  headerValueType _ = "string"

--------------------------------------------------------------------------------
-- DigestAuth
--------------------------------------------------------------------------------

-- | Digest authentication header value
-- |
-- | Example:
-- |   parseHeader "Digest username=\"user\", realm=\"api\", ..." :: Either String DigestAuth
-- |     = Right (DigestAuth "Digest username=\"user\", realm=\"api\", ...")
newtype DigestAuth = DigestAuth String

derive instance Eq DigestAuth
derive newtype instance Show DigestAuth
derive instance Newtype DigestAuth _

instance HeaderValue DigestAuth where
  parseHeader s =
    if String.take 7 s == "Digest " then Right $ DigestAuth s
    else Left "missing 'Digest ' prefix"

  printHeader (DigestAuth value) = value

instance HeaderValueType DigestAuth where
  headerValueType _ = "string"
