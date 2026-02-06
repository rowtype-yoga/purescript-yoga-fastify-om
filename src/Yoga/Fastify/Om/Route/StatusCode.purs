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

-- 1xx Informational
instance StatusCodeMap "continue" where
  statusCodeFor _ = StatusCode 100

instance StatusCodeMap "switchingProtocols" where
  statusCodeFor _ = StatusCode 101

instance StatusCodeMap "processing" where
  statusCodeFor _ = StatusCode 102

instance StatusCodeMap "earlyHints" where
  statusCodeFor _ = StatusCode 103

-- 2xx Success
instance StatusCodeMap "ok" where
  statusCodeFor _ = StatusCode 200

instance StatusCodeMap "created" where
  statusCodeFor _ = StatusCode 201

instance StatusCodeMap "accepted" where
  statusCodeFor _ = StatusCode 202

instance StatusCodeMap "nonAuthoritativeInformation" where
  statusCodeFor _ = StatusCode 203

instance StatusCodeMap "noContent" where
  statusCodeFor _ = StatusCode 204

instance StatusCodeMap "resetContent" where
  statusCodeFor _ = StatusCode 205

instance StatusCodeMap "partialContent" where
  statusCodeFor _ = StatusCode 206

instance StatusCodeMap "multiStatus" where
  statusCodeFor _ = StatusCode 207

instance StatusCodeMap "alreadyReported" where
  statusCodeFor _ = StatusCode 208

instance StatusCodeMap "imUsed" where
  statusCodeFor _ = StatusCode 226

-- 3xx Redirection
instance StatusCodeMap "multipleChoices" where
  statusCodeFor _ = StatusCode 300

instance StatusCodeMap "movedPermanently" where
  statusCodeFor _ = StatusCode 301

instance StatusCodeMap "found" where
  statusCodeFor _ = StatusCode 302

instance StatusCodeMap "seeOther" where
  statusCodeFor _ = StatusCode 303

instance StatusCodeMap "notModified" where
  statusCodeFor _ = StatusCode 304

instance StatusCodeMap "useProxy" where
  statusCodeFor _ = StatusCode 305

instance StatusCodeMap "temporaryRedirect" where
  statusCodeFor _ = StatusCode 307

instance StatusCodeMap "permanentRedirect" where
  statusCodeFor _ = StatusCode 308

-- 4xx Client Errors
instance StatusCodeMap "badRequest" where
  statusCodeFor _ = StatusCode 400

instance StatusCodeMap "unauthorized" where
  statusCodeFor _ = StatusCode 401

instance StatusCodeMap "paymentRequired" where
  statusCodeFor _ = StatusCode 402

instance StatusCodeMap "forbidden" where
  statusCodeFor _ = StatusCode 403

instance StatusCodeMap "notFound" where
  statusCodeFor _ = StatusCode 404

instance StatusCodeMap "methodNotAllowed" where
  statusCodeFor _ = StatusCode 405

instance StatusCodeMap "notAcceptable" where
  statusCodeFor _ = StatusCode 406

instance StatusCodeMap "proxyAuthenticationRequired" where
  statusCodeFor _ = StatusCode 407

instance StatusCodeMap "requestTimeout" where
  statusCodeFor _ = StatusCode 408

instance StatusCodeMap "conflict" where
  statusCodeFor _ = StatusCode 409

instance StatusCodeMap "gone" where
  statusCodeFor _ = StatusCode 410

instance StatusCodeMap "lengthRequired" where
  statusCodeFor _ = StatusCode 411

instance StatusCodeMap "preconditionFailed" where
  statusCodeFor _ = StatusCode 412

instance StatusCodeMap "payloadTooLarge" where
  statusCodeFor _ = StatusCode 413

instance StatusCodeMap "uriTooLong" where
  statusCodeFor _ = StatusCode 414

instance StatusCodeMap "unsupportedMediaType" where
  statusCodeFor _ = StatusCode 415

instance StatusCodeMap "rangeNotSatisfiable" where
  statusCodeFor _ = StatusCode 416

instance StatusCodeMap "expectationFailed" where
  statusCodeFor _ = StatusCode 417

instance StatusCodeMap "imATeapot" where
  statusCodeFor _ = StatusCode 418

instance StatusCodeMap "misdirectedRequest" where
  statusCodeFor _ = StatusCode 421

instance StatusCodeMap "unprocessableEntity" where
  statusCodeFor _ = StatusCode 422

instance StatusCodeMap "locked" where
  statusCodeFor _ = StatusCode 423

instance StatusCodeMap "failedDependency" where
  statusCodeFor _ = StatusCode 424

instance StatusCodeMap "tooEarly" where
  statusCodeFor _ = StatusCode 425

instance StatusCodeMap "upgradeRequired" where
  statusCodeFor _ = StatusCode 426

instance StatusCodeMap "preconditionRequired" where
  statusCodeFor _ = StatusCode 428

instance StatusCodeMap "tooManyRequests" where
  statusCodeFor _ = StatusCode 429

instance StatusCodeMap "requestHeaderFieldsTooLarge" where
  statusCodeFor _ = StatusCode 431

instance StatusCodeMap "unavailableForLegalReasons" where
  statusCodeFor _ = StatusCode 451

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

instance StatusCodeMap "httpVersionNotSupported" where
  statusCodeFor _ = StatusCode 505

instance StatusCodeMap "variantAlsoNegotiates" where
  statusCodeFor _ = StatusCode 506

instance StatusCodeMap "insufficientStorage" where
  statusCodeFor _ = StatusCode 507

instance StatusCodeMap "loopDetected" where
  statusCodeFor _ = StatusCode 508

instance StatusCodeMap "notExtended" where
  statusCodeFor _ = StatusCode 510

instance StatusCodeMap "networkAuthenticationRequired" where
  statusCodeFor _ = StatusCode 511

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Convert StatusCode to String for OpenAPI generation
statusCodeToString :: StatusCode -> String
statusCodeToString (StatusCode n) = show n
