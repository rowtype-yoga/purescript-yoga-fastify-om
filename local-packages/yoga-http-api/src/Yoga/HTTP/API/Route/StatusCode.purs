module Yoga.HTTP.API.Route.StatusCode
  ( class StatusCodeMap
  , class StatusCodeMapImpl
  , statusCodeForImpl
  , statusCodeFor
  , statusCodeToString
  , StatusCode(..)
  , class StatusCodeToLabel
  ) where

import Prelude

import Type.Proxy (Proxy)
import Data.Newtype (class Newtype)
import Prim.TypeError (class Fail, Text, Above, Quote, Beside)

newtype StatusCode = StatusCode Int

derive instance Newtype StatusCode _
derive newtype instance Show StatusCode
derive newtype instance Eq StatusCode
derive newtype instance Ord StatusCode

--------------------------------------------------------------------------------
-- Internal Implementation with Proxy Value Matching
--------------------------------------------------------------------------------

-- | Internal class that matches on Proxy types to avoid overlapping instances
class StatusCodeMapImpl (proxy :: Type) where
  statusCodeForImpl :: proxy -> StatusCode

-- 1xx Informational
instance StatusCodeMapImpl (Proxy "continue") where
  statusCodeForImpl _ = StatusCode 100

else instance StatusCodeMapImpl (Proxy "switchingProtocols") where
  statusCodeForImpl _ = StatusCode 101

else instance StatusCodeMapImpl (Proxy "processing") where
  statusCodeForImpl _ = StatusCode 102

else instance StatusCodeMapImpl (Proxy "earlyHints") where
  statusCodeForImpl _ = StatusCode 103

-- 2xx Success
else instance StatusCodeMapImpl (Proxy "ok") where
  statusCodeForImpl _ = StatusCode 200

else instance StatusCodeMapImpl (Proxy "created") where
  statusCodeForImpl _ = StatusCode 201

else instance StatusCodeMapImpl (Proxy "accepted") where
  statusCodeForImpl _ = StatusCode 202

else instance StatusCodeMapImpl (Proxy "nonAuthoritativeInformation") where
  statusCodeForImpl _ = StatusCode 203

else instance StatusCodeMapImpl (Proxy "noContent") where
  statusCodeForImpl _ = StatusCode 204

else instance StatusCodeMapImpl (Proxy "resetContent") where
  statusCodeForImpl _ = StatusCode 205

else instance StatusCodeMapImpl (Proxy "partialContent") where
  statusCodeForImpl _ = StatusCode 206

else instance StatusCodeMapImpl (Proxy "multiStatus") where
  statusCodeForImpl _ = StatusCode 207

else instance StatusCodeMapImpl (Proxy "alreadyReported") where
  statusCodeForImpl _ = StatusCode 208

else instance StatusCodeMapImpl (Proxy "imUsed") where
  statusCodeForImpl _ = StatusCode 226

-- 3xx Redirection
else instance StatusCodeMapImpl (Proxy "multipleChoices") where
  statusCodeForImpl _ = StatusCode 300

else instance StatusCodeMapImpl (Proxy "movedPermanently") where
  statusCodeForImpl _ = StatusCode 301

else instance StatusCodeMapImpl (Proxy "found") where
  statusCodeForImpl _ = StatusCode 302

else instance StatusCodeMapImpl (Proxy "seeOther") where
  statusCodeForImpl _ = StatusCode 303

else instance StatusCodeMapImpl (Proxy "notModified") where
  statusCodeForImpl _ = StatusCode 304

else instance StatusCodeMapImpl (Proxy "useProxy") where
  statusCodeForImpl _ = StatusCode 305

else instance StatusCodeMapImpl (Proxy "temporaryRedirect") where
  statusCodeForImpl _ = StatusCode 307

else instance StatusCodeMapImpl (Proxy "permanentRedirect") where
  statusCodeForImpl _ = StatusCode 308

-- 4xx Client Errors
else instance StatusCodeMapImpl (Proxy "badRequest") where
  statusCodeForImpl _ = StatusCode 400

else instance StatusCodeMapImpl (Proxy "unauthorized") where
  statusCodeForImpl _ = StatusCode 401

else instance StatusCodeMapImpl (Proxy "paymentRequired") where
  statusCodeForImpl _ = StatusCode 402

else instance StatusCodeMapImpl (Proxy "forbidden") where
  statusCodeForImpl _ = StatusCode 403

else instance StatusCodeMapImpl (Proxy "notFound") where
  statusCodeForImpl _ = StatusCode 404

else instance StatusCodeMapImpl (Proxy "methodNotAllowed") where
  statusCodeForImpl _ = StatusCode 405

else instance StatusCodeMapImpl (Proxy "notAcceptable") where
  statusCodeForImpl _ = StatusCode 406

else instance StatusCodeMapImpl (Proxy "proxyAuthenticationRequired") where
  statusCodeForImpl _ = StatusCode 407

else instance StatusCodeMapImpl (Proxy "requestTimeout") where
  statusCodeForImpl _ = StatusCode 408

else instance StatusCodeMapImpl (Proxy "conflict") where
  statusCodeForImpl _ = StatusCode 409

else instance StatusCodeMapImpl (Proxy "gone") where
  statusCodeForImpl _ = StatusCode 410

else instance StatusCodeMapImpl (Proxy "lengthRequired") where
  statusCodeForImpl _ = StatusCode 411

else instance StatusCodeMapImpl (Proxy "preconditionFailed") where
  statusCodeForImpl _ = StatusCode 412

else instance StatusCodeMapImpl (Proxy "payloadTooLarge") where
  statusCodeForImpl _ = StatusCode 413

else instance StatusCodeMapImpl (Proxy "uriTooLong") where
  statusCodeForImpl _ = StatusCode 414

else instance StatusCodeMapImpl (Proxy "unsupportedMediaType") where
  statusCodeForImpl _ = StatusCode 415

else instance StatusCodeMapImpl (Proxy "rangeNotSatisfiable") where
  statusCodeForImpl _ = StatusCode 416

else instance StatusCodeMapImpl (Proxy "expectationFailed") where
  statusCodeForImpl _ = StatusCode 417

else instance StatusCodeMapImpl (Proxy "imATeapot") where
  statusCodeForImpl _ = StatusCode 418

else instance StatusCodeMapImpl (Proxy "misdirectedRequest") where
  statusCodeForImpl _ = StatusCode 421

else instance StatusCodeMapImpl (Proxy "unprocessableEntity") where
  statusCodeForImpl _ = StatusCode 422

else instance StatusCodeMapImpl (Proxy "locked") where
  statusCodeForImpl _ = StatusCode 423

else instance StatusCodeMapImpl (Proxy "failedDependency") where
  statusCodeForImpl _ = StatusCode 424

else instance StatusCodeMapImpl (Proxy "tooEarly") where
  statusCodeForImpl _ = StatusCode 425

else instance StatusCodeMapImpl (Proxy "upgradeRequired") where
  statusCodeForImpl _ = StatusCode 426

else instance StatusCodeMapImpl (Proxy "preconditionRequired") where
  statusCodeForImpl _ = StatusCode 428

else instance StatusCodeMapImpl (Proxy "tooManyRequests") where
  statusCodeForImpl _ = StatusCode 429

else instance StatusCodeMapImpl (Proxy "requestHeaderFieldsTooLarge") where
  statusCodeForImpl _ = StatusCode 431

else instance StatusCodeMapImpl (Proxy "unavailableForLegalReasons") where
  statusCodeForImpl _ = StatusCode 451

-- 5xx Server Errors
else instance StatusCodeMapImpl (Proxy "internalServerError") where
  statusCodeForImpl _ = StatusCode 500

else instance StatusCodeMapImpl (Proxy "notImplemented") where
  statusCodeForImpl _ = StatusCode 501

else instance StatusCodeMapImpl (Proxy "badGateway") where
  statusCodeForImpl _ = StatusCode 502

else instance StatusCodeMapImpl (Proxy "serviceUnavailable") where
  statusCodeForImpl _ = StatusCode 503

else instance StatusCodeMapImpl (Proxy "gatewayTimeout") where
  statusCodeForImpl _ = StatusCode 504

else instance StatusCodeMapImpl (Proxy "httpVersionNotSupported") where
  statusCodeForImpl _ = StatusCode 505

else instance StatusCodeMapImpl (Proxy "variantAlsoNegotiates") where
  statusCodeForImpl _ = StatusCode 506

else instance StatusCodeMapImpl (Proxy "insufficientStorage") where
  statusCodeForImpl _ = StatusCode 507

else instance StatusCodeMapImpl (Proxy "loopDetected") where
  statusCodeForImpl _ = StatusCode 508

else instance StatusCodeMapImpl (Proxy "notExtended") where
  statusCodeForImpl _ = StatusCode 510

else instance StatusCodeMapImpl (Proxy "networkAuthenticationRequired") where
  statusCodeForImpl _ = StatusCode 511

-- Catch-all with helpful error message
else instance
  ( Fail
      ( Above
          (Beside (Text "Unknown status code: ") (Quote sym))
          ( Above
              (Text "")
              ( Above
                  (Text "Valid codes:")
                  ( Above
                      (Text "  • 2xx Success: ok, created, accepted, noContent, etc.")
                      ( Above
                          (Text "  • 3xx Redirect: movedPermanently, found, seeOther, notModified, etc.")
                          ( Above
                              (Text "  • 4xx Client: badRequest, unauthorized, forbidden, notFound, conflict, unprocessableEntity, etc.")
                              (Text "  • 5xx Server: internalServerError, badGateway, serviceUnavailable, etc.")
                          )
                      )
                  )
              )
          )
      )
  ) =>
  StatusCodeMapImpl (Proxy sym) where
  statusCodeForImpl _ = StatusCode 500

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

-- | Map variant constructor names (Symbols) to HTTP status codes
class StatusCodeMap (sym :: Symbol) where
  statusCodeFor :: Proxy sym -> StatusCode

-- Delegate to the internal implementation
instance StatusCodeMapImpl (Proxy sym) => StatusCodeMap sym where
  statusCodeFor proxy = statusCodeForImpl proxy

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Convert StatusCode to String for OpenAPI generation
statusCodeToString :: StatusCode -> String
statusCodeToString (StatusCode n) = show n

--------------------------------------------------------------------------------
-- Status Code to Label Mapping
--------------------------------------------------------------------------------

-- | Map numeric status codes to their label names
class StatusCodeToLabel (code :: Int) (label :: Symbol) | code -> label

-- 1xx Informational
instance StatusCodeToLabel 100 "continue"
instance StatusCodeToLabel 101 "switchingProtocols"
instance StatusCodeToLabel 102 "processing"
instance StatusCodeToLabel 103 "earlyHints"

-- 2xx Success
instance StatusCodeToLabel 200 "ok"
instance StatusCodeToLabel 201 "created"
instance StatusCodeToLabel 202 "accepted"
instance StatusCodeToLabel 203 "nonAuthoritativeInformation"
instance StatusCodeToLabel 204 "noContent"
instance StatusCodeToLabel 205 "resetContent"
instance StatusCodeToLabel 206 "partialContent"
instance StatusCodeToLabel 207 "multiStatus"
instance StatusCodeToLabel 208 "alreadyReported"
instance StatusCodeToLabel 226 "imUsed"

-- 3xx Redirection
instance StatusCodeToLabel 300 "multipleChoices"
instance StatusCodeToLabel 301 "movedPermanently"
instance StatusCodeToLabel 302 "found"
instance StatusCodeToLabel 303 "seeOther"
instance StatusCodeToLabel 304 "notModified"
instance StatusCodeToLabel 305 "useProxy"
instance StatusCodeToLabel 307 "temporaryRedirect"
instance StatusCodeToLabel 308 "permanentRedirect"

-- 4xx Client Errors
instance StatusCodeToLabel 400 "badRequest"
instance StatusCodeToLabel 401 "unauthorized"
instance StatusCodeToLabel 402 "paymentRequired"
instance StatusCodeToLabel 403 "forbidden"
instance StatusCodeToLabel 404 "notFound"
instance StatusCodeToLabel 405 "methodNotAllowed"
instance StatusCodeToLabel 406 "notAcceptable"
instance StatusCodeToLabel 407 "proxyAuthenticationRequired"
instance StatusCodeToLabel 408 "requestTimeout"
instance StatusCodeToLabel 409 "conflict"
instance StatusCodeToLabel 410 "gone"
instance StatusCodeToLabel 411 "lengthRequired"
instance StatusCodeToLabel 412 "preconditionFailed"
instance StatusCodeToLabel 413 "payloadTooLarge"
instance StatusCodeToLabel 414 "uriTooLong"
instance StatusCodeToLabel 415 "unsupportedMediaType"
instance StatusCodeToLabel 416 "rangeNotSatisfiable"
instance StatusCodeToLabel 417 "expectationFailed"
instance StatusCodeToLabel 418 "imATeapot"
instance StatusCodeToLabel 421 "misdirectedRequest"
instance StatusCodeToLabel 422 "unprocessableEntity"
instance StatusCodeToLabel 423 "locked"
instance StatusCodeToLabel 424 "failedDependency"
instance StatusCodeToLabel 425 "tooEarly"
instance StatusCodeToLabel 426 "upgradeRequired"
instance StatusCodeToLabel 428 "preconditionRequired"
instance StatusCodeToLabel 429 "tooManyRequests"
instance StatusCodeToLabel 431 "requestHeaderFieldsTooLarge"
instance StatusCodeToLabel 451 "unavailableForLegalReasons"

-- 5xx Server Errors
instance StatusCodeToLabel 500 "internalServerError"
instance StatusCodeToLabel 501 "notImplemented"
instance StatusCodeToLabel 502 "badGateway"
instance StatusCodeToLabel 503 "serviceUnavailable"
instance StatusCodeToLabel 504 "gatewayTimeout"
instance StatusCodeToLabel 505 "httpVersionNotSupported"
instance StatusCodeToLabel 506 "variantAlsoNegotiates"
instance StatusCodeToLabel 507 "insufficientStorage"
instance StatusCodeToLabel 508 "loopDetected"
instance StatusCodeToLabel 510 "notExtended"
instance StatusCodeToLabel 511 "networkAuthenticationRequired"
