module Yoga.Fastify.Om.Route.StatusCode
  ( class StatusCodeMap
  , statusCodeFor
  , statusCodeToString
  ) where

import Prelude

import Type.Proxy (Proxy(..))
import Yoga.Fastify.Fastify (StatusCode(..))

--------------------------------------------------------------------------------
-- Status Code Mapping
--------------------------------------------------------------------------------

-- | Map variant constructor names (Symbols) to HTTP status codes
-- |
-- | This typeclass enables convention-based status code mapping where
-- | the variant label name determines the HTTP status code.
-- |
-- | Users can extend this by adding their own instances for custom status codes.
class StatusCodeMap (sym :: Symbol) where
  statusCodeFor :: Proxy sym -> StatusCode

--------------------------------------------------------------------------------
-- Standard HTTP Status Code Instances
--------------------------------------------------------------------------------

-- 2xx Success
instance StatusCodeMap "ok" where
  statusCodeFor _ = StatusCode 200

instance StatusCodeMap "created" where
  statusCodeFor _ = StatusCode 201

instance StatusCodeMap "accepted" where
  statusCodeFor _ = StatusCode 202

instance StatusCodeMap "noContent" where
  statusCodeFor _ = StatusCode 204

-- 3xx Redirection
instance StatusCodeMap "movedPermanently" where
  statusCodeFor _ = StatusCode 301

instance StatusCodeMap "found" where
  statusCodeFor _ = StatusCode 302

instance StatusCodeMap "seeOther" where
  statusCodeFor _ = StatusCode 303

instance StatusCodeMap "notModified" where
  statusCodeFor _ = StatusCode 304

instance StatusCodeMap "temporaryRedirect" where
  statusCodeFor _ = StatusCode 307

instance StatusCodeMap "permanentRedirect" where
  statusCodeFor _ = StatusCode 308

-- 4xx Client Errors
instance StatusCodeMap "badRequest" where
  statusCodeFor _ = StatusCode 400

instance StatusCodeMap "unauthorized" where
  statusCodeFor _ = StatusCode 401

instance StatusCodeMap "forbidden" where
  statusCodeFor _ = StatusCode 403

instance StatusCodeMap "notFound" where
  statusCodeFor _ = StatusCode 404

instance StatusCodeMap "methodNotAllowed" where
  statusCodeFor _ = StatusCode 405

instance StatusCodeMap "notAcceptable" where
  statusCodeFor _ = StatusCode 406

instance StatusCodeMap "conflict" where
  statusCodeFor _ = StatusCode 409

instance StatusCodeMap "gone" where
  statusCodeFor _ = StatusCode 410

instance StatusCodeMap "preconditionFailed" where
  statusCodeFor _ = StatusCode 412

instance StatusCodeMap "payloadTooLarge" where
  statusCodeFor _ = StatusCode 413

instance StatusCodeMap "unsupportedMediaType" where
  statusCodeFor _ = StatusCode 415

instance StatusCodeMap "unprocessableEntity" where
  statusCodeFor _ = StatusCode 422

instance StatusCodeMap "tooManyRequests" where
  statusCodeFor _ = StatusCode 429

-- 5xx Server Errors
instance StatusCodeMap "internalServerError" where
  statusCodeFor _ = StatusCode 500

instance StatusCodeMap "notImplemented" where
  statusCodeFor _ = StatusCode 501

instance StatusCodeMap "badGateway" where
  statusCodeFor _ = StatusCode 502

instance StatusCodeMap "serviceUnavailable" where
  statusCodeFor _ = StatusCode 503

instance StatusCodeMap "gatewayTimeout" where
  statusCodeFor _ = StatusCode 504

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Convert StatusCode to String for OpenAPI generation
statusCodeToString :: StatusCode -> String
statusCodeToString (StatusCode n) = show n
