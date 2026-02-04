module Yoga.Fastify.Om
  ( -- * Request-in-Context Types (NEW!)
    RequestContext
  , OmHandler
  , OmFastify
  -- * Om-aware server setup (NEW!)
  , createOmFastify
  -- * Om-aware route registration with request in context (NEW!)
  , getOm
  , postOm
  , putOm
  , deleteOm
  , patchOm
  -- * Low-level request data accessors from context (NEW!)
  , httpRequest
  , requestHeaders
  , requestParams
  , requestQuery
  , requestBody
  , requestMethod
  , requestUrl
  -- * High-level typed helpers (NEW!)
  , MissingParam
  , ParamErrors
  , MissingHeader
  , HeaderErrors
  , MissingQueryParam
  , QueryParamErrors
  , requiredParam
  , param
  , requiredParams
  , optionalParams
  , class ParseParam
  , parseParam
  , class ParseParams
  , parseParams
  , class ParseOptionalParams
  , parseOptionalParams
  , class ParseQueryParams
  , parseQueryParams
  , class ParseOptionalQueryParams
  , parseOptionalQueryParams
  , requiredHeader
  , requestHeader
  , requiredHeaders
  , optionalHeaders
  , requiredQueryParam
  , queryParam
  , requiredQueryParams
  , optionalQueryParams
  -- * Legacy Om-friendly server operations (kept for backwards compatibility)
  , listen
  , close
  , get
  , post
  , put
  , delete
  , patch
  , route
  -- * Legacy Om-friendly request operations (kept for backwards compatibility)
  , body
  , params
  , query
  , headers
  , method
  , url
  -- * Om-friendly reply operations
  , status
  , header
  , send
  , sendJson
  -- * Re-exports from base module
  , module Yoga.Fastify.Fastify
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect (Effect)
import Foreign (Foreign)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks, class Union)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Fastify (Fastify, FastifyRequest, FastifyReply, RouteHandler, Host, Port, HTTPMethod, RouteURL, StatusCode)
import Yoga.Om (Om)
import Yoga.Om as Om

--------------------------------------------------------------------------------
-- REQUEST-IN-CONTEXT TYPES
--------------------------------------------------------------------------------

-- | Request context that gets injected automatically into Om context as `httpRequest` field
-- | Contains all request data: headers, params, query, body, method, url
type RequestContext =
  { headers :: Object String
  , params :: Object String
  , query :: Object Foreign -- Use yoga-json to decode query params
  , body :: Maybe Foreign
  , method :: HTTPMethod
  , url :: RouteURL
  }

-- | Om handler type: receives only reply, request data is in context as `httpRequest`!
-- | Access request data via helpers: `hdrs <- requestHeaders` or `{ httpRequest } <- ask`
type OmHandler ctx err = FastifyReply -> Om ctx err Unit

-- | Wrapper for Fastify app with stored application context
-- | Used by Om-aware route registration (getOm, postOm, etc.)
-- | The app context must not contain an `httpRequest` field (enforced by Lacks constraint)
type OmFastify appCtx =
  { fastify :: Fastify
  , contextRef :: Ref { | appCtx }
  }

--------------------------------------------------------------------------------
-- OM-AWARE FASTIFY SETUP
--------------------------------------------------------------------------------

-- | Create Om-aware Fastify wrapper that stores application context
-- |
-- | Example:
-- |   app <- liftEffect $ F.fastify {}
-- |   omApp <- liftEffect $ createOmFastify { db, logger } app
-- |
-- | Note: Your app context must not contain an `httpRequest` field (compile-time enforced)
createOmFastify
  :: forall appCtx
   . Lacks "httpRequest" appCtx
  => { | appCtx }
  -> Fastify
  -> Effect (OmFastify appCtx)
createOmFastify appContext fastifyApp = do
  contextRef <- Ref.new appContext
  pure { fastify: fastifyApp, contextRef }

--------------------------------------------------------------------------------
-- INTERNAL: WRAP OM HANDLER FOR BASE FASTIFY
--------------------------------------------------------------------------------

-- | Internal helper: wraps OmHandler to work with base Fastify RouteHandler
-- | Extracts request data, inserts as `httpRequest` field, runs Om handler
-- | Uses unsafeCoerce to bypass rigid type variable issues (safe due to Lacks constraint)
wrapOmHandler
  :: forall appCtx fullCtx
   . Lacks "httpRequest" appCtx
  => Cons "httpRequest" RequestContext appCtx fullCtx
  => Ref { | appCtx }
  -> OmHandler { | fullCtx } ()
  -> RouteHandler
wrapOmHandler contextRef omHandler = \req reply -> do
  -- Extract all request data from FastifyRequest
  hdrs <- liftEffect $ F.headers req
  ps <- liftEffect $ F.params req
  q <- liftEffect $ F.query req
  b <- liftEffect $ F.body req
  m <- liftEffect $ F.method req
  u <- liftEffect $ F.url req

  -- Build request context record
  let
    requestContext :: RequestContext
    requestContext = { headers: hdrs, params: ps, query: q, body: b, method: m, url: u }

  -- Get app context from ref
  appContext <- liftEffect $ Ref.read contextRef

  -- Insert httpRequest field into app context
  -- unsafeCoerce is safe here: Lacks ensures no collision, Cons describes the result type
  let
    fullContext :: { | fullCtx }
    fullContext = unsafeCoerce (Record.insert (Proxy :: _ "httpRequest") requestContext appContext)

  -- Run Om handler with full context (app + httpRequest)
  Om.runOm fullContext { exception: Aff.throwError } (omHandler reply)

--------------------------------------------------------------------------------
-- OM-AWARE ROUTE REGISTRATION (with httpRequest in context!)
--------------------------------------------------------------------------------

-- | Register GET route with Om handler (httpRequest injected into context)
-- |
-- | Example:
-- |   getOm (RouteURL "/users/:id") handler omApp
-- |   where
-- |   handler reply = do
-- |     { httpRequest } <- ask  -- Request data in context!
-- |     -- Or use helpers:
-- |     hdrs <- requestHeaders
getOm
  :: forall appCtx fullCtx
   . Lacks "httpRequest" appCtx
  => Cons "httpRequest" RequestContext appCtx fullCtx
  => RouteURL
  -> OmHandler { | fullCtx } ()
  -> OmFastify appCtx
  -> Effect Unit
getOm routeUrl omHandler omApp =
  F.route
    { method: F.HTTPMethod "GET", url: routeUrl }
    (unsafeCoerce (wrapOmHandler omApp.contextRef) omHandler)
    omApp.fastify

-- | Register POST route with Om handler (httpRequest injected into context)
postOm
  :: forall appCtx fullCtx
   . Lacks "httpRequest" appCtx
  => Cons "httpRequest" RequestContext appCtx fullCtx
  => RouteURL
  -> OmHandler { | fullCtx } ()
  -> OmFastify appCtx
  -> Effect Unit
postOm routeUrl omHandler omApp =
  F.route
    { method: F.HTTPMethod "POST", url: routeUrl }
    (unsafeCoerce (wrapOmHandler omApp.contextRef) omHandler)
    omApp.fastify

-- | Register PUT route with Om handler (httpRequest injected into context)
putOm
  :: forall appCtx fullCtx
   . Lacks "httpRequest" appCtx
  => Cons "httpRequest" RequestContext appCtx fullCtx
  => RouteURL
  -> OmHandler { | fullCtx } ()
  -> OmFastify appCtx
  -> Effect Unit
putOm routeUrl omHandler omApp =
  F.route
    { method: F.HTTPMethod "PUT", url: routeUrl }
    (unsafeCoerce (wrapOmHandler omApp.contextRef) omHandler)
    omApp.fastify

-- | Register DELETE route with Om handler (httpRequest injected into context)
deleteOm
  :: forall appCtx fullCtx
   . Lacks "httpRequest" appCtx
  => Cons "httpRequest" RequestContext appCtx fullCtx
  => RouteURL
  -> OmHandler { | fullCtx } ()
  -> OmFastify appCtx
  -> Effect Unit
deleteOm routeUrl omHandler omApp =
  F.route
    { method: F.HTTPMethod "DELETE", url: routeUrl }
    (unsafeCoerce (wrapOmHandler omApp.contextRef) omHandler)
    omApp.fastify

-- | Register PATCH route with Om handler (httpRequest injected into context)
patchOm
  :: forall appCtx fullCtx
   . Lacks "httpRequest" appCtx
  => Cons "httpRequest" RequestContext appCtx fullCtx
  => RouteURL
  -> OmHandler { | fullCtx } ()
  -> OmFastify appCtx
  -> Effect Unit
patchOm routeUrl omHandler omApp =
  F.route
    { method: F.HTTPMethod "PATCH", url: routeUrl }
    (unsafeCoerce (wrapOmHandler omApp.contextRef) omHandler)
    omApp.fastify

--------------------------------------------------------------------------------
-- REQUEST DATA ACCESSORS (from context via ask)
--------------------------------------------------------------------------------

-- | Get full HTTP request context from Om context
-- |
-- | Example:
-- |   req <- httpRequest
-- |   let hdrs = req.headers
httpRequest
  :: forall ctx err rest
   . Cons "httpRequest" RequestContext rest ctx
  => Om { | ctx } err RequestContext
-- Note: unsafeCoerce is safe here - the Cons constraint proves the field exists at compile-time
httpRequest = Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest

-- | Get headers from Om context (no parameter needed!)
-- |
-- | Example:
-- |   hdrs <- requestHeaders
-- |   case Object.lookup "authorization" hdrs of ...
requestHeaders
  :: forall ctx err rest
   . Cons "httpRequest" RequestContext rest ctx
  => Om { | ctx } err (Object String)
-- Note: unsafeCoerce is safe here - the Cons constraint proves the field exists at compile-time
requestHeaders = Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.headers

-- | Get path params from Om context (no parameter needed!)
-- |
-- | Example:
-- |   ps <- requestParams
-- |   case Object.lookup "id" ps of ...
requestParams
  :: forall ctx err rest
   . Cons "httpRequest" RequestContext rest ctx
  => Om { | ctx } err (Object String)
-- Note: unsafeCoerce is safe here - the Cons constraint proves the field exists at compile-time
requestParams = Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.params

-- | Get query params from Om context (no parameter needed!)
-- |
-- | Example:
-- |   q <- requestQuery
-- |   case Object.lookup "limit" q of ...
requestQuery
  :: forall ctx err rest
   . Cons "httpRequest" RequestContext rest ctx
  => Om { | ctx } err (Object Foreign)
-- Note: unsafeCoerce is safe here - the Cons constraint proves the field exists at compile-time
requestQuery = Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.query

-- | Get request body from Om context (no parameter needed!)
-- |
-- | Example:
-- |   bodyMaybe <- requestBody
-- |   case bodyMaybe of ...
requestBody
  :: forall ctx err rest
   . Cons "httpRequest" RequestContext rest ctx
  => Om { | ctx } err (Maybe Foreign)
-- Note: unsafeCoerce is safe here - the Cons constraint proves the field exists at compile-time
requestBody = Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.body

-- | Get HTTP method from Om context (no parameter needed!)
requestMethod
  :: forall ctx err rest
   . Cons "httpRequest" RequestContext rest ctx
  => Om { | ctx } err HTTPMethod
-- Note: unsafeCoerce is safe here - the Cons constraint proves the field exists at compile-time
requestMethod = Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.method

-- | Get request URL from Om context (no parameter needed!)
requestUrl
  :: forall ctx err rest
   . Cons "httpRequest" RequestContext rest ctx
  => Om { | ctx } err RouteURL
-- Note: unsafeCoerce is safe here - the Cons constraint proves the field exists at compile-time
requestUrl = Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.url

--------------------------------------------------------------------------------
-- HIGH-LEVEL TYPED HELPERS
--------------------------------------------------------------------------------

-- | Error type for missing path parameter
type MissingParam = { missingParam :: String }

-- | Error type for path parameter errors (missing or invalid)
type ParamErrors = { missing :: Array String, invalid :: Array String }

-- | Error type for missing header
type MissingHeader = { missingHeader :: String }

-- | Error type for header errors (missing or invalid)
type HeaderErrors = { missing :: Array String, invalid :: Array String }

-- | Error type for missing query parameter
type MissingQueryParam = { missingQueryParam :: String }

-- | Error type for query parameter errors (missing or invalid)
type QueryParamErrors = { missing :: Array String, invalid :: Array String }

-- | Get a required path parameter, throws MissingParam if not found
-- |
-- | Example:
-- |   userId <- requiredParam "id"
requiredParam
  :: forall ctx rest
   . Cons "httpRequest" RequestContext rest ctx
  => String
  -> Om { | ctx } (missingParam :: String) String
requiredParam key = do
  ps <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.params
  case Object.lookup key ps of
    Just value -> pure value
    Nothing -> Om.throw { missingParam: key }

-- | Get an optional path parameter
-- |
-- | Example:
-- |   userIdMaybe <- param "id"
param
  :: forall ctx err rest
   . Cons "httpRequest" RequestContext rest ctx
  => String
  -> Om { | ctx } err (Maybe String)
param key = do
  ps <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.params
  pure $ Object.lookup key ps

-- | Parse a String to a specific type (used internally by requiredParams)
class ParseParam a where
  parseParam :: String -> Maybe a

instance ParseParam String where
  parseParam = Just

instance ParseParam Int where
  parseParam = Int.fromString

instance ParseParam Number where
  parseParam s = case Int.fromString s of
    Just n -> Just (Int.toNumber n)
    Nothing -> Nothing -- TODO: parse actual floats

-- | Typeclass to parse multiple params into a record, accumulating all errors
class ParseParams (rl :: RowList Type) (r :: Row Type) | rl -> r where
  parseParams :: Proxy rl -> Object String -> Either { missing :: Array String, invalid :: Array String } (Record r)

instance ParseParams RL.Nil () where
  parseParams _ _ = Right {}

instance
  ( IsSymbol name
  , ParseParam ty
  , ParseParams tail tailRow
  , Cons name ty tailRow r
  , Lacks name tailRow
  ) =>
  ParseParams (RL.Cons name ty tail) r where
  parseParams _ ps =
    let
      key = Proxy :: Proxy name
      keyName = reflectSymbol key
      -- Check if field exists
      valueResult = case Object.lookup keyName ps of
        Nothing -> Left { missing: [ keyName ], invalid: [] }
        Just valueStr -> case parseParam valueStr of
          Nothing -> Left { missing: [], invalid: [ keyName ] }
          Just value -> Right value
      -- Parse the rest
      restResult = parseParams (Proxy :: Proxy tail) ps
    in
      case valueResult, restResult of
        Right value, Right rest -> Right (Record.insert key value rest)
        Left err1, Left err2 -> Left
          { missing: err1.missing <> err2.missing
          , invalid: err1.invalid <> err2.invalid
          }
        Left err, Right _ -> Left err
        Right _, Left err -> Left err

-- | Get multiple required path parameters as a typed record
-- | Throws ParamErrors with all missing and invalid fields
-- |
-- | Example:
-- |   { userId, postId } <- requiredParams (Proxy :: _ { userId :: Int, postId :: String })
requiredParams
  :: forall ctx rest r rl
   . Cons "httpRequest" RequestContext rest ctx
  => RowToList r rl
  => ParseParams rl r
  => Proxy r
  -> Om { | ctx } (paramErrors :: ParamErrors) (Record r)
requiredParams _ = do
  ps <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.params
  case parseParams (Proxy :: Proxy rl) ps of
    Right parsed -> pure parsed
    Left errors -> Om.throw { paramErrors: errors }

-- | Typeclass to parse multiple optional params into a record with Maybe fields
class ParseOptionalParams (rl :: RowList Type) (r :: Row Type) | rl -> r where
  parseOptionalParams :: Proxy rl -> Object String -> Record r

instance ParseOptionalParams RL.Nil () where
  parseOptionalParams _ _ = {}

instance
  ( IsSymbol name
  , ParseParam ty
  , ParseOptionalParams tail tailRow
  , Cons name (Maybe ty) tailRow r
  , Lacks name tailRow
  ) =>
  ParseOptionalParams (RL.Cons name ty tail) r where
  parseOptionalParams _ ps =
    let
      key = Proxy :: Proxy name
      keyName = reflectSymbol key
      value = Object.lookup keyName ps >>= parseParam
      rest = parseOptionalParams (Proxy :: Proxy tail) ps
    in
      Record.insert key value rest

-- | Get multiple optional path parameters as a typed record with Maybe fields
-- |
-- | Example:
-- |   { userId, postId } <- optionalParams (Proxy :: _ { userId :: Int, postId :: String })
-- |   -- Returns { userId :: Maybe Int, postId :: Maybe String }
optionalParams
  :: forall ctx rest r rl
   . Cons "httpRequest" RequestContext rest ctx
  => RowToList r rl
  => ParseOptionalParams rl r
  => Proxy r
  -> Om { | ctx } () (Record r)
optionalParams _ = do
  ps <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.params
  pure $ parseOptionalParams (Proxy :: Proxy rl) ps

-- | Get a required header, throws MissingHeader if not found
-- |
-- | Example:
-- |   authToken <- requiredHeader "authorization"
requiredHeader
  :: forall ctx rest
   . Cons "httpRequest" RequestContext rest ctx
  => String
  -> Om { | ctx } (missingHeader :: String) String
requiredHeader key = do
  hdrs <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.headers
  case Object.lookup key hdrs of
    Just value -> pure value
    Nothing -> Om.throw { missingHeader: key }

-- | Get an optional header
-- |
-- | Example:
-- |   authTokenMaybe <- requestHeader "authorization"
requestHeader
  :: forall ctx err rest
   . Cons "httpRequest" RequestContext rest ctx
  => String
  -> Om { | ctx } err (Maybe String)
requestHeader key = do
  hdrs <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.headers
  pure $ Object.lookup key hdrs

-- | Get multiple required headers as a typed record
-- | Throws HeaderErrors with all missing and invalid fields
-- |
-- | Example:
-- |   { authorization, contentType } <- requiredHeaders (Proxy :: _ { authorization :: String, contentType :: String })
requiredHeaders
  :: forall ctx rest r rl
   . Cons "httpRequest" RequestContext rest ctx
  => RowToList r rl
  => ParseParams rl r
  => Proxy r
  -> Om { | ctx } (headerErrors :: HeaderErrors) (Record r)
requiredHeaders _ = do
  hdrs <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.headers
  case parseParams (Proxy :: Proxy rl) hdrs of
    Right parsed -> pure parsed
    Left errors -> Om.throw { headerErrors: errors }

-- | Get multiple optional headers as a typed record with Maybe fields
-- |
-- | Example:
-- |   { authorization, userAgent } <- optionalHeaders (Proxy :: _ { authorization :: String, userAgent :: String })
-- |   -- Returns { authorization :: Maybe String, userAgent :: Maybe String }
optionalHeaders
  :: forall ctx rest r rl
   . Cons "httpRequest" RequestContext rest ctx
  => RowToList r rl
  => ParseOptionalParams rl r
  => Proxy r
  -> Om { | ctx } () (Record r)
optionalHeaders _ = do
  hdrs <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.headers
  pure $ parseOptionalParams (Proxy :: Proxy rl) hdrs

-- | Get a required query parameter, throws MissingQueryParam if not found
-- | Returns Foreign - use yoga-json to decode
-- |
-- | Example:
-- |   limitForeign <- requiredQueryParam "limit"
-- |   limit <- liftEffect $ J.readJSON limitForeign
requiredQueryParam
  :: forall ctx rest
   . Cons "httpRequest" RequestContext rest ctx
  => String
  -> Om { | ctx } (missingQueryParam :: String) Foreign
requiredQueryParam key = do
  q <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.query
  case Object.lookup key q of
    Just value -> pure value
    Nothing -> Om.throw { missingQueryParam: key }

-- | Get an optional query parameter
-- |
-- | Example:
-- |   limitMaybe <- queryParam "limit"
queryParam
  :: forall ctx err rest
   . Cons "httpRequest" RequestContext rest ctx
  => String
  -> Om { | ctx } err (Maybe Foreign)
queryParam key = do
  q <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.query
  pure $ Object.lookup key q

-- | Typeclass to parse query params (Foreign) into a record, accumulating all errors
-- |
-- | ARCHITECTURAL NOTE:
-- | Query params are typed as `Object Foreign` to match TypeScript's `unknown` type.
-- | Currently, we use `unsafeCoerce` to convert Foreign -> String for the default
-- | Fastify querystring parser (which produces strings/string arrays).
-- |
-- | FUTURE: Integrate yoga-json's ReadForeign typeclass for proper Foreign decoding:
-- |   - Remove ParseParam dependency
-- |   - Use J.readJSON or J.readImpl for type-safe Foreign -> ty conversion
-- |   - Handle complex query param types (nested objects, arrays, etc.)
class ParseQueryParams (rl :: RowList Type) (r :: Row Type) | rl -> r where
  parseQueryParams :: Proxy rl -> Object Foreign -> Either { missing :: Array String, invalid :: Array String } (Record r)

instance ParseQueryParams RL.Nil () where
  parseQueryParams _ _ = Right {}

instance
  ( IsSymbol name
  , ParseParam ty
  , ParseQueryParams tail tailRow
  , Cons name ty tailRow r
  , Lacks name tailRow
  ) =>
  ParseQueryParams (RL.Cons name ty tail) r where
  parseQueryParams _ qps =
    let
      key = Proxy :: Proxy name
      keyName = reflectSymbol key
      -- Check if field exists and parse it (Foreign -> ty via ParseParam)
      -- Note: unsafeCoerce Foreign -> String is safe for default Fastify querystring parser
      -- TODO: Replace with yoga-json's ReadForeign for proper type-safe decoding
      valueResult = case Object.lookup keyName qps of
        Nothing -> Left { missing: [ keyName ], invalid: [] }
        Just foreignVal ->
          let
            valueStr = unsafeCoerce foreignVal :: String -- Safe: default parser produces strings
          in
            case parseParam valueStr of
              Nothing -> Left { missing: [], invalid: [ keyName ] }
              Just value -> Right value
      -- Parse the rest
      restResult = parseQueryParams (Proxy :: Proxy tail) qps
    in
      case valueResult, restResult of
        Right value, Right rest -> Right (Record.insert key value rest)
        Left err1, Left err2 -> Left
          { missing: err1.missing <> err2.missing
          , invalid: err1.invalid <> err2.invalid
          }
        Left err, Right _ -> Left err
        Right _, Left err -> Left err

-- | Get multiple required query parameters as a typed record
-- | Throws QueryParamErrors with all missing and invalid fields
-- |
-- | Example:
-- |   { page, limit } <- requiredQueryParams (Proxy :: _ { page :: Int, limit :: Int })
requiredQueryParams
  :: forall ctx rest r rl
   . Cons "httpRequest" RequestContext rest ctx
  => RowToList r rl
  => ParseQueryParams rl r
  => Proxy r
  -> Om { | ctx } (queryParamErrors :: QueryParamErrors) (Record r)
requiredQueryParams _ = do
  q <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.query
  case parseQueryParams (Proxy :: Proxy rl) q of
    Right parsed -> pure parsed
    Left errors -> Om.throw { queryParamErrors: errors }

-- | Typeclass to parse optional query params (Foreign) into a record with Maybe fields
-- | TODO: Integrate yoga-json's ReadForeign (see ParseQueryParams note)
class ParseOptionalQueryParams (rl :: RowList Type) (r :: Row Type) | rl -> r where
  parseOptionalQueryParams :: Proxy rl -> Object Foreign -> Record r

instance ParseOptionalQueryParams RL.Nil () where
  parseOptionalQueryParams _ _ = {}

instance
  ( IsSymbol name
  , ParseParam ty
  , ParseOptionalQueryParams tail tailRow
  , Cons name (Maybe ty) tailRow r
  , Lacks name tailRow
  ) =>
  ParseOptionalQueryParams (RL.Cons name ty tail) r where
  parseOptionalQueryParams _ qps =
    let
      key = Proxy :: Proxy name
      keyName = reflectSymbol key
      value = do
        foreignVal <- Object.lookup keyName qps
        -- TODO: Replace with yoga-json's ReadForeign for proper type-safe decoding
        let valueStr = unsafeCoerce foreignVal :: String -- Safe: default parser produces strings
        parseParam valueStr
      rest = parseOptionalQueryParams (Proxy :: Proxy tail) qps
    in
      Record.insert key value rest

-- | Get multiple optional query parameters as a typed record with Maybe fields
-- |
-- | Example:
-- |   { page, limit } <- optionalQueryParams (Proxy :: _ { page :: Int, limit :: Int })
-- |   -- Returns { page :: Maybe Int, limit :: Maybe Int }
optionalQueryParams
  :: forall ctx rest r rl
   . Cons "httpRequest" RequestContext rest ctx
  => RowToList r rl
  => ParseOptionalQueryParams rl r
  => Proxy r
  -> Om { | ctx } () (Record r)
optionalQueryParams _ = do
  q <- Om.ask <#> \rec -> (unsafeCoerce rec).httpRequest.query
  pure $ parseOptionalQueryParams (Proxy :: Proxy rl) q

--------------------------------------------------------------------------------
-- LEGACY OM-FRIENDLY SERVER OPERATIONS (backwards compatibility)
--------------------------------------------------------------------------------

-- | Om-friendly listen
listen :: forall ctx err opts opts_. Union opts opts_ F.ListenOptionsImpl => { | opts } -> F.Fastify -> Om ctx err String
listen opts app = liftAff $ F.listen opts app

-- | Om-friendly close
close :: forall ctx err. F.Fastify -> Om ctx err Unit
close = liftAff <<< F.close

-- | Om-friendly GET route
get :: forall ctx err. F.RouteURL -> F.RouteHandler -> F.Fastify -> Om ctx err Unit
get routeUrl handler app = liftEffect $ F.get routeUrl handler app

-- | Om-friendly POST route
post :: forall ctx err. F.RouteURL -> F.RouteHandler -> F.Fastify -> Om ctx err Unit
post routeUrl handler app = liftEffect $ F.post routeUrl handler app

-- | Om-friendly PUT route
put :: forall ctx err. F.RouteURL -> F.RouteHandler -> F.Fastify -> Om ctx err Unit
put routeUrl handler app = liftEffect $ F.put routeUrl handler app

-- | Om-friendly DELETE route
delete :: forall ctx err. F.RouteURL -> F.RouteHandler -> F.Fastify -> Om ctx err Unit
delete routeUrl handler app = liftEffect $ F.delete routeUrl handler app

-- | Om-friendly PATCH route
patch :: forall ctx err. F.RouteURL -> F.RouteHandler -> F.Fastify -> Om ctx err Unit
patch routeUrl handler app = liftEffect $ F.patch routeUrl handler app

-- | Om-friendly route registration
route
  :: forall ctx err opts opts_
   . Union opts opts_ F.RouteOptionsImpl
  => { | opts }
  -> F.RouteHandler
  -> F.Fastify
  -> Om ctx err Unit
route opts handler app = liftEffect $ F.route opts handler app

-- Request API

-- | Om-friendly body
body :: forall ctx err. F.FastifyRequest -> Om ctx err (Maybe Foreign)
body = liftEffect <<< F.body

-- | Om-friendly params
params :: forall ctx err. F.FastifyRequest -> Om ctx err (Object String)
params = liftEffect <<< F.params

-- | Om-friendly query
query :: forall ctx err. F.FastifyRequest -> Om ctx err (Object Foreign)
query = liftEffect <<< F.query

-- | Alias for query (kept for backwards compatibility)
queryString :: forall ctx err. F.FastifyRequest -> Om ctx err (Object Foreign)
queryString = query

-- | Om-friendly headers
headers :: forall ctx err. F.FastifyRequest -> Om ctx err (Object String)
headers = liftEffect <<< F.headers

-- | Om-friendly method
method :: forall ctx err. F.FastifyRequest -> Om ctx err F.HTTPMethod
method = liftEffect <<< F.method

-- | Om-friendly url
url :: forall ctx err. F.FastifyRequest -> Om ctx err F.RouteURL
url = liftEffect <<< F.url

-- Reply API

-- | Om-friendly status
status :: forall ctx err. F.StatusCode -> F.FastifyReply -> Om ctx err F.FastifyReply
status code reply = liftEffect $ F.status code reply

-- | Om-friendly header
header :: forall ctx err. String -> String -> F.FastifyReply -> Om ctx err F.FastifyReply
header key value reply = liftEffect $ F.header key value reply

-- | Om-friendly send
send :: forall ctx err. Foreign -> F.FastifyReply -> Om ctx err Unit
send payload reply = liftAff $ F.send payload reply

-- | Om-friendly sendJson
sendJson :: forall ctx err. Foreign -> F.FastifyReply -> Om ctx err Unit
sendJson payload reply = liftAff $ F.sendJson payload reply
