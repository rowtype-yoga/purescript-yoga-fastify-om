module Yoga.HTTP.API.Route.Response
  ( Response(..)
  , ResponseData
  , ResponseHeaders
  , ResponseBody
  , EmptyResponse
  , class ToResponse
  , class ToResponseRL
  , class IsUnitBody
  , respond
  , respondWith
  , respondNoHeaders
  , respondStatus
  , respondStatusWith
  , respondNoBody
  , respondNothing
  , respondNoContent
  , respondNotModified
  -- Status code helpers
  , continue
  , continueWith
  , switchingProtocols
  , switchingProtocolsWith
  , processing
  , processingWith
  , earlyHints
  , earlyHintsWith
  , ok
  , okWith
  , created
  , createdWith
  , accepted
  , acceptedWith
  , nonAuthoritativeInformation
  , nonAuthoritativeInformationWith
  , noContent
  , noContentWith
  , resetContent
  , resetContentWith
  , partialContent
  , partialContentWith
  , multiStatus
  , multiStatusWith
  , alreadyReported
  , alreadyReportedWith
  , imUsed
  , imUsedWith
  , multipleChoices
  , multipleChoicesWith
  , movedPermanently
  , movedPermanentlyWith
  , found
  , foundWith
  , seeOther
  , seeOtherWith
  , notModified
  , notModifiedWith
  , useProxy
  , useProxyWith
  , temporaryRedirect
  , temporaryRedirectWith
  , permanentRedirect
  , permanentRedirectWith
  , badRequest
  , badRequestWith
  , unauthorized
  , unauthorizedWith
  , paymentRequired
  , paymentRequiredWith
  , forbidden
  , forbiddenWith
  , notFound
  , notFoundWith
  , methodNotAllowed
  , methodNotAllowedWith
  , notAcceptable
  , notAcceptableWith
  , proxyAuthenticationRequired
  , proxyAuthenticationRequiredWith
  , requestTimeout
  , requestTimeoutWith
  , conflict
  , conflictWith
  , gone
  , goneWith
  , lengthRequired
  , lengthRequiredWith
  , preconditionFailed
  , preconditionFailedWith
  , payloadTooLarge
  , payloadTooLargeWith
  , uriTooLong
  , uriTooLongWith
  , unsupportedMediaType
  , unsupportedMediaTypeWith
  , rangeNotSatisfiable
  , rangeNotSatisfiableWith
  , expectationFailed
  , expectationFailedWith
  , imATeapot
  , imATeapotWith
  , misdirectedRequest
  , misdirectedRequestWith
  , unprocessableEntity
  , unprocessableEntityWith
  , locked
  , lockedWith
  , failedDependency
  , failedDependencyWith
  , tooEarly
  , tooEarlyWith
  , upgradeRequired
  , upgradeRequiredWith
  , preconditionRequired
  , preconditionRequiredWith
  , tooManyRequests
  , tooManyRequestsWith
  , requestHeaderFieldsTooLarge
  , requestHeaderFieldsTooLargeWith
  , unavailableForLegalReasons
  , unavailableForLegalReasonsWith
  , internalServerError
  , internalServerErrorWith
  , notImplemented
  , notImplementedWith
  , badGateway
  , badGatewayWith
  , serviceUnavailable
  , serviceUnavailableWith
  , gatewayTimeout
  , gatewayTimeoutWith
  , httpVersionNotSupported
  , httpVersionNotSupportedWith
  , variantAlsoNegotiates
  , variantAlsoNegotiatesWith
  , insufficientStorage
  , insufficientStorageWith
  , loopDetected
  , loopDetectedWith
  , notExtended
  , notExtendedWith
  , networkAuthenticationRequired
  , networkAuthenticationRequiredWith
  , module Data.Variant
  ) where

import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Boolean (True, False)
import Prim.Row (class Cons)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text, Above)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.HTTP.API.Route.StatusCode (class StatusCodeToLabel)

--------------------------------------------------------------------------------
-- Response Data Type
--------------------------------------------------------------------------------

-- | Response data combining headers and body
-- | This is a data type (not type alias) to work with type class instances
data Response headers body = Response
  { headers :: Record headers
  , body :: body
  }

type ResponseBody body = Response () body

type ResponseHeaders headers = Response headers Unit

type EmptyResponse = Response () Unit

-- | Deprecated alias for backwards compatibility
type ResponseData headers body = Response headers body

--------------------------------------------------------------------------------
-- ToResponse: Convert record syntax to Response type
--------------------------------------------------------------------------------

-- | Convert user-friendly record syntax to Response type.
-- |
-- | Supports:
-- |   Response headers body → Response headers body (identity)
-- |   { body :: User } → Response () User
-- |   { headers :: { "Location" :: String }, body :: User } → Response ("Location" :: String) User
class ToResponse (recordType :: Type) (headers :: Row Type) (body :: Type) | recordType -> headers body

-- Identity instance: Response is already Response
instance toResponseIdentity :: ToResponse (Response headers body) headers body

-- Record instance: delegate to RowList-based helper
else instance toResponseRecord ::
  ( RL.RowToList recordRow rl
  , ToResponseRL rl headers body
  ) =>
  ToResponse (Record recordRow) headers body

-- | RowList-based helper to distinguish records with/without headers.
-- | Instance heads are distinguishable by the RowList structure.
class ToResponseRL (rl :: RL.RowList Type) (headers :: Row Type) (body :: Type) | rl -> headers body

-- { body :: b, headers :: Record h } (headers comes before body in sorted RowList)
instance toResponseRLBodyHeaders ::
  ToResponseRL (RL.Cons "body" body (RL.Cons "headers" (Record headers) RL.Nil)) headers body

-- { body :: b } (no headers)
else instance toResponseRLBodyOnly ::
  ToResponseRL (RL.Cons "body" body RL.Nil) () body

-- { headers :: Record h } (no body)
else instance ToResponseRL (RL.Cons "headers" (Record headers) RL.Nil) headers Unit

-- {} (empty record: no body, no headers)
else instance ToResponseRL RL.Nil () Unit

-- Invalid record structure
else instance
  ( Fail
      ( Above
          (Text "Invalid response record structure.")
          ( Above
              (Text "")
              ( Above
                  (Text "Response records must contain ONLY:")
                  ( Above
                      (Text "  • body :: YourBodyType              (optional)")
                      ( Above
                          (Text "  • headers :: Record (...)         (optional)")
                          ( Above
                              (Text "")
                              ( Above
                                  (Text "Valid examples:")
                                  ( Above
                                      (Text "  { body: user }")
                                      ( Above
                                          (Text "  { body: user, headers: { \"Location\": \"/users/123\" } }")
                                          ( Above
                                              (Text "  { headers: { \"X-Exists\": \"true\" } }")
                                              ( Above
                                                  (Text "  {}")
                                                  ( Above
                                                      (Text "")
                                                      (Text "Do not include 'status', 'statusCode', or other fields.")
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          )
      )
  ) =>
  ToResponseRL rl headers body

--------------------------------------------------------------------------------
-- IsUnitBody: Type-level Boolean dispatch for body presence
--------------------------------------------------------------------------------

class IsUnitBody (body :: Type) (isUnit :: Boolean) | body -> isUnit
instance IsUnitBody Unit True
else instance IsUnitBody body False

--------------------------------------------------------------------------------
-- Response Construction Helpers
--------------------------------------------------------------------------------

-- | Construct a variant response with full control over headers and body
-- |
-- | Example:
-- |   respond (Proxy :: _ "created")
-- |     (Response { headers: { "Location": "/users/123" }, body: user })
respond
  :: forall label headers body r1 r2
   . IsSymbol label
  => Cons label (Response headers body) r1 r2
  => Proxy label
  -> Response headers body
  -> Variant r2
respond label rd = Variant.inj label rd

-- | Construct a variant response with separate headers and body arguments
-- |
-- | Example:
-- |   respondWith (Proxy :: _ "created")
-- |     { "Location": "/users/123" }
-- |     user
respondWith
  :: forall label headers body r1 r2
   . IsSymbol label
  => Cons label (Response headers body) r1 r2
  => Proxy label
  -> Record headers
  -> body
  -> Variant r2
respondWith label headers body =
  Variant.inj label (Response { headers, body })

-- | Construct a variant response with no custom headers (most common case)
-- |
-- | Example:
-- |   respondNoHeaders (Proxy :: _ "ok") user
-- |   respondNoHeaders (Proxy :: _ "notFound") { error: "Not found" }
respondNoHeaders
  :: forall @label body r1 r2
   . IsSymbol label
  => Cons label (Response () body) r1 r2
  => body
  -> Variant r2
respondNoHeaders body =
  Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body })

-- | Construct a variant response using numeric status code
-- |
-- | Example:
-- |   respondStatus @200 user
-- |   respondStatus @404 { error: "Not found" }
respondStatus
  :: forall @code label body r1 r2
   . StatusCodeToLabel code label
  => IsSymbol label
  => Cons label (Response () body) r1 r2
  => body
  -> Variant r2
respondStatus body =
  Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body })

-- | Construct a variant response with headers using numeric status code
-- |
-- | Example:
-- |   respondStatusWith @201 { "Location": "/users/123" } user
respondStatusWith
  :: forall @code label headers body r1 r2
   . StatusCodeToLabel code label
  => IsSymbol label
  => Cons label (Response headers body) r1 r2
  => Record headers
  -> body
  -> Variant r2
respondStatusWith headers body =
  Variant.inj (Proxy :: Proxy label) (Response { headers, body })

-- | Construct a variant response with headers but no body
-- |
-- | Example:
-- |   respondNoBody (Proxy :: _ "noContent")
-- |     { "X-Resource-Id": "123" }
respondNoBody
  :: forall label headers r1 r2
   . IsSymbol label
  => Cons label (Response headers Unit) r1 r2
  => Proxy label
  -> Record headers
  -> Variant r2
respondNoBody label headers =
  Variant.inj label (Response { headers, body: unit })

-- | Construct a variant response with neither headers nor body
-- |
-- | Example:
-- |   respondNothing (Proxy :: _ "noContent")
-- |   respondNothing (Proxy :: _ "notModified")
respondNothing
  :: forall @label r1 r2
   . IsSymbol label
  => Cons label (Response () Unit) r1 r2
  => Variant r2
respondNothing =
  Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body: unit })

-- | Construct a 204 No Content response (no headers, no body)
-- |
-- | Example:
-- |   respondNoContent
respondNoContent
  :: forall r1 r2
   . Cons "noContent" (Response () Unit) r1 r2
  => Variant r2
respondNoContent =
  unsafeCoerce (Variant.inj (Proxy :: Proxy "noContent") (Response { headers: {}, body: unit }))

-- | Construct a 304 Not Modified response (no headers, no body)
-- |
-- | Example:
-- |   respondNotModified
respondNotModified
  :: forall r1 r2
   . Cons "notModified" (Response () Unit) r1 r2
  => Variant r2
respondNotModified =
  unsafeCoerce (Variant.inj (Proxy :: Proxy "notModified") (Response { headers: {}, body: unit }))

--------------------------------------------------------------------------------
-- Status Code Helper Functions
--------------------------------------------------------------------------------

continue :: forall body r1. body -> Variant (continue :: Response () body | r1)
continue = respondNoHeaders @"continue"

continueWith :: forall headers body r1. Record headers -> body -> Variant (continue :: Response headers body | r1)
continueWith headers body = respondWith (Proxy :: Proxy "continue") headers body

switchingProtocols :: forall body r1. body -> Variant (switchingProtocols :: Response () body | r1)
switchingProtocols = respondNoHeaders @"switchingProtocols"

switchingProtocolsWith :: forall headers body r1. Record headers -> body -> Variant (switchingProtocols :: Response headers body | r1)
switchingProtocolsWith headers body = respondWith (Proxy :: Proxy "switchingProtocols") headers body

processing :: forall body r1. body -> Variant (processing :: Response () body | r1)
processing = respondNoHeaders @"processing"

processingWith :: forall headers body r1. Record headers -> body -> Variant (processing :: Response headers body | r1)
processingWith headers body = respondWith (Proxy :: Proxy "processing") headers body

earlyHints :: forall body r1. body -> Variant (earlyHints :: Response () body | r1)
earlyHints = respondNoHeaders @"earlyHints"

earlyHintsWith :: forall headers body r1. Record headers -> body -> Variant (earlyHints :: Response headers body | r1)
earlyHintsWith headers body = respondWith (Proxy :: Proxy "earlyHints") headers body

ok :: forall body r1. body -> Variant (ok :: Response () body | r1)
ok = respondNoHeaders @"ok"

okWith :: forall headers body r1. Record headers -> body -> Variant (ok :: Response headers body | r1)
okWith headers body = respondWith (Proxy :: Proxy "ok") headers body

created :: forall body r1. body -> Variant (created :: Response () body | r1)
created = respondNoHeaders @"created"

createdWith :: forall headers body r1. Record headers -> body -> Variant (created :: Response headers body | r1)
createdWith headers body = respondWith (Proxy :: Proxy "created") headers body

accepted :: forall body r1. body -> Variant (accepted :: Response () body | r1)
accepted = respondNoHeaders @"accepted"

acceptedWith :: forall headers body r1. Record headers -> body -> Variant (accepted :: Response headers body | r1)
acceptedWith headers body = respondWith (Proxy :: Proxy "accepted") headers body

nonAuthoritativeInformation :: forall body r1. body -> Variant (nonAuthoritativeInformation :: Response () body | r1)
nonAuthoritativeInformation = respondNoHeaders @"nonAuthoritativeInformation"

nonAuthoritativeInformationWith :: forall headers body r1. Record headers -> body -> Variant (nonAuthoritativeInformation :: Response headers body | r1)
nonAuthoritativeInformationWith headers body = respondWith (Proxy :: Proxy "nonAuthoritativeInformation") headers body

noContent :: forall body r1. body -> Variant (noContent :: Response () body | r1)
noContent = respondNoHeaders @"noContent"

noContentWith :: forall headers body r1. Record headers -> body -> Variant (noContent :: Response headers body | r1)
noContentWith headers body = respondWith (Proxy :: Proxy "noContent") headers body

resetContent :: forall body r1. body -> Variant (resetContent :: Response () body | r1)
resetContent = respondNoHeaders @"resetContent"

resetContentWith :: forall headers body r1. Record headers -> body -> Variant (resetContent :: Response headers body | r1)
resetContentWith headers body = respondWith (Proxy :: Proxy "resetContent") headers body

partialContent :: forall body r1. body -> Variant (partialContent :: Response () body | r1)
partialContent = respondNoHeaders @"partialContent"

partialContentWith :: forall headers body r1. Record headers -> body -> Variant (partialContent :: Response headers body | r1)
partialContentWith headers body = respondWith (Proxy :: Proxy "partialContent") headers body

multiStatus :: forall body r1. body -> Variant (multiStatus :: Response () body | r1)
multiStatus = respondNoHeaders @"multiStatus"

multiStatusWith :: forall headers body r1. Record headers -> body -> Variant (multiStatus :: Response headers body | r1)
multiStatusWith headers body = respondWith (Proxy :: Proxy "multiStatus") headers body

alreadyReported :: forall body r1. body -> Variant (alreadyReported :: Response () body | r1)
alreadyReported = respondNoHeaders @"alreadyReported"

alreadyReportedWith :: forall headers body r1. Record headers -> body -> Variant (alreadyReported :: Response headers body | r1)
alreadyReportedWith headers body = respondWith (Proxy :: Proxy "alreadyReported") headers body

imUsed :: forall body r1. body -> Variant (imUsed :: Response () body | r1)
imUsed = respondNoHeaders @"imUsed"

imUsedWith :: forall headers body r1. Record headers -> body -> Variant (imUsed :: Response headers body | r1)
imUsedWith headers body = respondWith (Proxy :: Proxy "imUsed") headers body

multipleChoices :: forall body r1. body -> Variant (multipleChoices :: Response () body | r1)
multipleChoices = respondNoHeaders @"multipleChoices"

multipleChoicesWith :: forall headers body r1. Record headers -> body -> Variant (multipleChoices :: Response headers body | r1)
multipleChoicesWith headers body = respondWith (Proxy :: Proxy "multipleChoices") headers body

movedPermanently :: forall body r1. body -> Variant (movedPermanently :: Response () body | r1)
movedPermanently = respondNoHeaders @"movedPermanently"

movedPermanentlyWith :: forall headers body r1. Record headers -> body -> Variant (movedPermanently :: Response headers body | r1)
movedPermanentlyWith headers body = respondWith (Proxy :: Proxy "movedPermanently") headers body

found :: forall body r1. body -> Variant (found :: Response () body | r1)
found = respondNoHeaders @"found"

foundWith :: forall headers body r1. Record headers -> body -> Variant (found :: Response headers body | r1)
foundWith headers body = respondWith (Proxy :: Proxy "found") headers body

seeOther :: forall body r1. body -> Variant (seeOther :: Response () body | r1)
seeOther = respondNoHeaders @"seeOther"

seeOtherWith :: forall headers body r1. Record headers -> body -> Variant (seeOther :: Response headers body | r1)
seeOtherWith headers body = respondWith (Proxy :: Proxy "seeOther") headers body

notModified :: forall body r1. body -> Variant (notModified :: Response () body | r1)
notModified = respondNoHeaders @"notModified"

notModifiedWith :: forall headers body r1. Record headers -> body -> Variant (notModified :: Response headers body | r1)
notModifiedWith headers body = respondWith (Proxy :: Proxy "notModified") headers body

useProxy :: forall body r1. body -> Variant (useProxy :: Response () body | r1)
useProxy = respondNoHeaders @"useProxy"

useProxyWith :: forall headers body r1. Record headers -> body -> Variant (useProxy :: Response headers body | r1)
useProxyWith headers body = respondWith (Proxy :: Proxy "useProxy") headers body

temporaryRedirect :: forall body r1. body -> Variant (temporaryRedirect :: Response () body | r1)
temporaryRedirect = respondNoHeaders @"temporaryRedirect"

temporaryRedirectWith :: forall headers body r1. Record headers -> body -> Variant (temporaryRedirect :: Response headers body | r1)
temporaryRedirectWith headers body = respondWith (Proxy :: Proxy "temporaryRedirect") headers body

permanentRedirect :: forall body r1. body -> Variant (permanentRedirect :: Response () body | r1)
permanentRedirect = respondNoHeaders @"permanentRedirect"

permanentRedirectWith :: forall headers body r1. Record headers -> body -> Variant (permanentRedirect :: Response headers body | r1)
permanentRedirectWith headers body = respondWith (Proxy :: Proxy "permanentRedirect") headers body

badRequest :: forall body r1. body -> Variant (badRequest :: Response () body | r1)
badRequest = respondNoHeaders @"badRequest"

badRequestWith :: forall headers body r1. Record headers -> body -> Variant (badRequest :: Response headers body | r1)
badRequestWith headers body = respondWith (Proxy :: Proxy "badRequest") headers body

unauthorized :: forall body r1. body -> Variant (unauthorized :: Response () body | r1)
unauthorized = respondNoHeaders @"unauthorized"

unauthorizedWith :: forall headers body r1. Record headers -> body -> Variant (unauthorized :: Response headers body | r1)
unauthorizedWith headers body = respondWith (Proxy :: Proxy "unauthorized") headers body

paymentRequired :: forall body r1. body -> Variant (paymentRequired :: Response () body | r1)
paymentRequired = respondNoHeaders @"paymentRequired"

paymentRequiredWith :: forall headers body r1. Record headers -> body -> Variant (paymentRequired :: Response headers body | r1)
paymentRequiredWith headers body = respondWith (Proxy :: Proxy "paymentRequired") headers body

forbidden :: forall body r1. body -> Variant (forbidden :: Response () body | r1)
forbidden = respondNoHeaders @"forbidden"

forbiddenWith :: forall headers body r1. Record headers -> body -> Variant (forbidden :: Response headers body | r1)
forbiddenWith headers body = respondWith (Proxy :: Proxy "forbidden") headers body

notFound :: forall body r1. body -> Variant (notFound :: Response () body | r1)
notFound = respondNoHeaders @"notFound"

notFoundWith :: forall headers body r1. Record headers -> body -> Variant (notFound :: Response headers body | r1)
notFoundWith headers body = respondWith (Proxy :: Proxy "notFound") headers body

methodNotAllowed :: forall body r1. body -> Variant (methodNotAllowed :: Response () body | r1)
methodNotAllowed = respondNoHeaders @"methodNotAllowed"

methodNotAllowedWith :: forall headers body r1. Record headers -> body -> Variant (methodNotAllowed :: Response headers body | r1)
methodNotAllowedWith headers body = respondWith (Proxy :: Proxy "methodNotAllowed") headers body

notAcceptable :: forall body r1. body -> Variant (notAcceptable :: Response () body | r1)
notAcceptable = respondNoHeaders @"notAcceptable"

notAcceptableWith :: forall headers body r1. Record headers -> body -> Variant (notAcceptable :: Response headers body | r1)
notAcceptableWith headers body = respondWith (Proxy :: Proxy "notAcceptable") headers body

proxyAuthenticationRequired :: forall body r1. body -> Variant (proxyAuthenticationRequired :: Response () body | r1)
proxyAuthenticationRequired = respondNoHeaders @"proxyAuthenticationRequired"

proxyAuthenticationRequiredWith :: forall headers body r1. Record headers -> body -> Variant (proxyAuthenticationRequired :: Response headers body | r1)
proxyAuthenticationRequiredWith headers body = respondWith (Proxy :: Proxy "proxyAuthenticationRequired") headers body

requestTimeout :: forall body r1. body -> Variant (requestTimeout :: Response () body | r1)
requestTimeout = respondNoHeaders @"requestTimeout"

requestTimeoutWith :: forall headers body r1. Record headers -> body -> Variant (requestTimeout :: Response headers body | r1)
requestTimeoutWith headers body = respondWith (Proxy :: Proxy "requestTimeout") headers body

conflict :: forall body r1. body -> Variant (conflict :: Response () body | r1)
conflict = respondNoHeaders @"conflict"

conflictWith :: forall headers body r1. Record headers -> body -> Variant (conflict :: Response headers body | r1)
conflictWith headers body = respondWith (Proxy :: Proxy "conflict") headers body

gone :: forall body r1. body -> Variant (gone :: Response () body | r1)
gone = respondNoHeaders @"gone"

goneWith :: forall headers body r1. Record headers -> body -> Variant (gone :: Response headers body | r1)
goneWith headers body = respondWith (Proxy :: Proxy "gone") headers body

lengthRequired :: forall body r1. body -> Variant (lengthRequired :: Response () body | r1)
lengthRequired = respondNoHeaders @"lengthRequired"

lengthRequiredWith :: forall headers body r1. Record headers -> body -> Variant (lengthRequired :: Response headers body | r1)
lengthRequiredWith headers body = respondWith (Proxy :: Proxy "lengthRequired") headers body

preconditionFailed :: forall body r1. body -> Variant (preconditionFailed :: Response () body | r1)
preconditionFailed = respondNoHeaders @"preconditionFailed"

preconditionFailedWith :: forall headers body r1. Record headers -> body -> Variant (preconditionFailed :: Response headers body | r1)
preconditionFailedWith headers body = respondWith (Proxy :: Proxy "preconditionFailed") headers body

payloadTooLarge :: forall body r1. body -> Variant (payloadTooLarge :: Response () body | r1)
payloadTooLarge = respondNoHeaders @"payloadTooLarge"

payloadTooLargeWith :: forall headers body r1. Record headers -> body -> Variant (payloadTooLarge :: Response headers body | r1)
payloadTooLargeWith headers body = respondWith (Proxy :: Proxy "payloadTooLarge") headers body

uriTooLong :: forall body r1. body -> Variant (uriTooLong :: Response () body | r1)
uriTooLong = respondNoHeaders @"uriTooLong"

uriTooLongWith :: forall headers body r1. Record headers -> body -> Variant (uriTooLong :: Response headers body | r1)
uriTooLongWith headers body = respondWith (Proxy :: Proxy "uriTooLong") headers body

unsupportedMediaType :: forall body r1. body -> Variant (unsupportedMediaType :: Response () body | r1)
unsupportedMediaType = respondNoHeaders @"unsupportedMediaType"

unsupportedMediaTypeWith :: forall headers body r1. Record headers -> body -> Variant (unsupportedMediaType :: Response headers body | r1)
unsupportedMediaTypeWith headers body = respondWith (Proxy :: Proxy "unsupportedMediaType") headers body

rangeNotSatisfiable :: forall body r1. body -> Variant (rangeNotSatisfiable :: Response () body | r1)
rangeNotSatisfiable = respondNoHeaders @"rangeNotSatisfiable"

rangeNotSatisfiableWith :: forall headers body r1. Record headers -> body -> Variant (rangeNotSatisfiable :: Response headers body | r1)
rangeNotSatisfiableWith headers body = respondWith (Proxy :: Proxy "rangeNotSatisfiable") headers body

expectationFailed :: forall body r1. body -> Variant (expectationFailed :: Response () body | r1)
expectationFailed = respondNoHeaders @"expectationFailed"

expectationFailedWith :: forall headers body r1. Record headers -> body -> Variant (expectationFailed :: Response headers body | r1)
expectationFailedWith headers body = respondWith (Proxy :: Proxy "expectationFailed") headers body

imATeapot :: forall body r1. body -> Variant (imATeapot :: Response () body | r1)
imATeapot = respondNoHeaders @"imATeapot"

imATeapotWith :: forall headers body r1. Record headers -> body -> Variant (imATeapot :: Response headers body | r1)
imATeapotWith headers body = respondWith (Proxy :: Proxy "imATeapot") headers body

misdirectedRequest :: forall body r1. body -> Variant (misdirectedRequest :: Response () body | r1)
misdirectedRequest = respondNoHeaders @"misdirectedRequest"

misdirectedRequestWith :: forall headers body r1. Record headers -> body -> Variant (misdirectedRequest :: Response headers body | r1)
misdirectedRequestWith headers body = respondWith (Proxy :: Proxy "misdirectedRequest") headers body

unprocessableEntity :: forall body r1. body -> Variant (unprocessableEntity :: Response () body | r1)
unprocessableEntity = respondNoHeaders @"unprocessableEntity"

unprocessableEntityWith :: forall headers body r1. Record headers -> body -> Variant (unprocessableEntity :: Response headers body | r1)
unprocessableEntityWith headers body = respondWith (Proxy :: Proxy "unprocessableEntity") headers body

locked :: forall body r1. body -> Variant (locked :: Response () body | r1)
locked = respondNoHeaders @"locked"

lockedWith :: forall headers body r1. Record headers -> body -> Variant (locked :: Response headers body | r1)
lockedWith headers body = respondWith (Proxy :: Proxy "locked") headers body

failedDependency :: forall body r1. body -> Variant (failedDependency :: Response () body | r1)
failedDependency = respondNoHeaders @"failedDependency"

failedDependencyWith :: forall headers body r1. Record headers -> body -> Variant (failedDependency :: Response headers body | r1)
failedDependencyWith headers body = respondWith (Proxy :: Proxy "failedDependency") headers body

tooEarly :: forall body r1. body -> Variant (tooEarly :: Response () body | r1)
tooEarly = respondNoHeaders @"tooEarly"

tooEarlyWith :: forall headers body r1. Record headers -> body -> Variant (tooEarly :: Response headers body | r1)
tooEarlyWith headers body = respondWith (Proxy :: Proxy "tooEarly") headers body

upgradeRequired :: forall body r1. body -> Variant (upgradeRequired :: Response () body | r1)
upgradeRequired = respondNoHeaders @"upgradeRequired"

upgradeRequiredWith :: forall headers body r1. Record headers -> body -> Variant (upgradeRequired :: Response headers body | r1)
upgradeRequiredWith headers body = respondWith (Proxy :: Proxy "upgradeRequired") headers body

preconditionRequired :: forall body r1. body -> Variant (preconditionRequired :: Response () body | r1)
preconditionRequired = respondNoHeaders @"preconditionRequired"

preconditionRequiredWith :: forall headers body r1. Record headers -> body -> Variant (preconditionRequired :: Response headers body | r1)
preconditionRequiredWith headers body = respondWith (Proxy :: Proxy "preconditionRequired") headers body

tooManyRequests :: forall body r1. body -> Variant (tooManyRequests :: Response () body | r1)
tooManyRequests = respondNoHeaders @"tooManyRequests"

tooManyRequestsWith :: forall headers body r1. Record headers -> body -> Variant (tooManyRequests :: Response headers body | r1)
tooManyRequestsWith headers body = respondWith (Proxy :: Proxy "tooManyRequests") headers body

requestHeaderFieldsTooLarge :: forall body r1. body -> Variant (requestHeaderFieldsTooLarge :: Response () body | r1)
requestHeaderFieldsTooLarge = respondNoHeaders @"requestHeaderFieldsTooLarge"

requestHeaderFieldsTooLargeWith :: forall headers body r1. Record headers -> body -> Variant (requestHeaderFieldsTooLarge :: Response headers body | r1)
requestHeaderFieldsTooLargeWith headers body = respondWith (Proxy :: Proxy "requestHeaderFieldsTooLarge") headers body

unavailableForLegalReasons :: forall body r1. body -> Variant (unavailableForLegalReasons :: Response () body | r1)
unavailableForLegalReasons = respondNoHeaders @"unavailableForLegalReasons"

unavailableForLegalReasonsWith :: forall headers body r1. Record headers -> body -> Variant (unavailableForLegalReasons :: Response headers body | r1)
unavailableForLegalReasonsWith headers body = respondWith (Proxy :: Proxy "unavailableForLegalReasons") headers body

internalServerError :: forall body r1. body -> Variant (internalServerError :: Response () body | r1)
internalServerError = respondNoHeaders @"internalServerError"

internalServerErrorWith :: forall headers body r1. Record headers -> body -> Variant (internalServerError :: Response headers body | r1)
internalServerErrorWith headers body = respondWith (Proxy :: Proxy "internalServerError") headers body

notImplemented :: forall body r1. body -> Variant (notImplemented :: Response () body | r1)
notImplemented = respondNoHeaders @"notImplemented"

notImplementedWith :: forall headers body r1. Record headers -> body -> Variant (notImplemented :: Response headers body | r1)
notImplementedWith headers body = respondWith (Proxy :: Proxy "notImplemented") headers body

badGateway :: forall body r1. body -> Variant (badGateway :: Response () body | r1)
badGateway = respondNoHeaders @"badGateway"

badGatewayWith :: forall headers body r1. Record headers -> body -> Variant (badGateway :: Response headers body | r1)
badGatewayWith headers body = respondWith (Proxy :: Proxy "badGateway") headers body

serviceUnavailable :: forall body r1. body -> Variant (serviceUnavailable :: Response () body | r1)
serviceUnavailable = respondNoHeaders @"serviceUnavailable"

serviceUnavailableWith :: forall headers body r1. Record headers -> body -> Variant (serviceUnavailable :: Response headers body | r1)
serviceUnavailableWith headers body = respondWith (Proxy :: Proxy "serviceUnavailable") headers body

gatewayTimeout :: forall body r1. body -> Variant (gatewayTimeout :: Response () body | r1)
gatewayTimeout = respondNoHeaders @"gatewayTimeout"

gatewayTimeoutWith :: forall headers body r1. Record headers -> body -> Variant (gatewayTimeout :: Response headers body | r1)
gatewayTimeoutWith headers body = respondWith (Proxy :: Proxy "gatewayTimeout") headers body

httpVersionNotSupported :: forall body r1. body -> Variant (httpVersionNotSupported :: Response () body | r1)
httpVersionNotSupported = respondNoHeaders @"httpVersionNotSupported"

httpVersionNotSupportedWith :: forall headers body r1. Record headers -> body -> Variant (httpVersionNotSupported :: Response headers body | r1)
httpVersionNotSupportedWith headers body = respondWith (Proxy :: Proxy "httpVersionNotSupported") headers body

variantAlsoNegotiates :: forall body r1. body -> Variant (variantAlsoNegotiates :: Response () body | r1)
variantAlsoNegotiates = respondNoHeaders @"variantAlsoNegotiates"

variantAlsoNegotiatesWith :: forall headers body r1. Record headers -> body -> Variant (variantAlsoNegotiates :: Response headers body | r1)
variantAlsoNegotiatesWith headers body = respondWith (Proxy :: Proxy "variantAlsoNegotiates") headers body

insufficientStorage :: forall body r1. body -> Variant (insufficientStorage :: Response () body | r1)
insufficientStorage = respondNoHeaders @"insufficientStorage"

insufficientStorageWith :: forall headers body r1. Record headers -> body -> Variant (insufficientStorage :: Response headers body | r1)
insufficientStorageWith headers body = respondWith (Proxy :: Proxy "insufficientStorage") headers body

loopDetected :: forall body r1. body -> Variant (loopDetected :: Response () body | r1)
loopDetected = respondNoHeaders @"loopDetected"

loopDetectedWith :: forall headers body r1. Record headers -> body -> Variant (loopDetected :: Response headers body | r1)
loopDetectedWith headers body = respondWith (Proxy :: Proxy "loopDetected") headers body

notExtended :: forall body r1. body -> Variant (notExtended :: Response () body | r1)
notExtended = respondNoHeaders @"notExtended"

notExtendedWith :: forall headers body r1. Record headers -> body -> Variant (notExtended :: Response headers body | r1)
notExtendedWith headers body = respondWith (Proxy :: Proxy "notExtended") headers body

networkAuthenticationRequired :: forall body r1. body -> Variant (networkAuthenticationRequired :: Response () body | r1)
networkAuthenticationRequired = respondNoHeaders @"networkAuthenticationRequired"

networkAuthenticationRequiredWith :: forall headers body r1. Record headers -> body -> Variant (networkAuthenticationRequired :: Response headers body | r1)
networkAuthenticationRequiredWith headers body = respondWith (Proxy :: Proxy "networkAuthenticationRequired") headers body
