module Yoga.Fastify.Om
  ( RequestContext
  , OmHandler
  , OmFastify
  , createOmFastify
  , getOm
  , postOm
  , putOm
  , deleteOm
  , patchOm
  , httpRequest
  , requestHeaders
  , requestParams
  , requestQuery
  , requestBody
  , requestMethod
  , requestUrl
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
  , module Yoga.Fastify.Fastify
  ) where

import Prelude

import Control.Monad.Except (runExcept)
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
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks)
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Fastify (Fastify, FastifyRequest, FastifyReply, RouteHandler, Host, Port, HTTPMethod, RouteURL, StatusCode)
import Yoga.JSON (readImpl)
import Yoga.Om (Om)
import Yoga.Om as Om

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

-- | Internal helper: wraps OmHandler to work with base Fastify RouteHandler
-- | Extracts request data, inserts as `httpRequest` field, runs Om handler
wrapOmHandler
  :: forall appCtx
   . Lacks "httpRequest" appCtx
  => Ref (Record appCtx)
  -> OmHandler { httpRequest :: RequestContext | appCtx } ()
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
  let
    fullContext = Record.insert (Proxy :: _ "httpRequest") requestContext appContext

  -- Run Om handler with full context (app + httpRequest)
  Om.runOm fullContext { exception: Aff.throwError } (omHandler reply)

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
  :: forall appCtx
   . Lacks "httpRequest" appCtx
  => RouteURL
  -> OmHandler { httpRequest :: RequestContext | appCtx } ()
  -> OmFastify appCtx
  -> Effect Unit
getOm routeUrl omHandler omApp = F.route
  { method: F.HTTPMethod "GET", url: routeUrl }
  (wrapOmHandler omApp.contextRef omHandler)
  omApp.fastify

-- | Register POST route with Om handler (httpRequest injected into context)
postOm
  :: forall appCtx
   . Lacks "httpRequest" appCtx
  => RouteURL
  -> OmHandler { httpRequest :: RequestContext | appCtx } ()
  -> OmFastify appCtx
  -> Effect Unit
postOm routeUrl omHandler omApp = F.route
  { method: F.HTTPMethod "POST", url: routeUrl }
  (wrapOmHandler omApp.contextRef omHandler)
  omApp.fastify

-- | Register PUT route with Om handler (httpRequest injected into context)
putOm
  :: forall appCtx
   . Lacks "httpRequest" appCtx
  => RouteURL
  -> OmHandler { httpRequest :: RequestContext | appCtx } ()
  -> OmFastify appCtx
  -> Effect Unit
putOm routeUrl omHandler omApp = F.route
  { method: F.HTTPMethod "PUT", url: routeUrl }
  (wrapOmHandler omApp.contextRef omHandler)
  omApp.fastify

-- | Register DELETE route with Om handler (httpRequest injected into context)
deleteOm
  :: forall appCtx
   . Lacks "httpRequest" appCtx
  => RouteURL
  -> OmHandler { httpRequest :: RequestContext | appCtx } ()
  -> OmFastify appCtx
  -> Effect Unit
deleteOm routeUrl omHandler omApp = F.route
  { method: F.HTTPMethod "DELETE", url: routeUrl }
  (wrapOmHandler omApp.contextRef omHandler)
  omApp.fastify

-- | Register PATCH route with Om handler (httpRequest injected into context)
patchOm
  :: forall appCtx
   . Lacks "httpRequest" appCtx
  => RouteURL
  -> OmHandler { httpRequest :: RequestContext | appCtx } ()
  -> OmFastify appCtx
  -> Effect Unit
patchOm routeUrl omHandler omApp = F.route
  { method: F.HTTPMethod "PATCH", url: routeUrl }
  (wrapOmHandler omApp.contextRef omHandler)
  omApp.fastify

-- | Get full HTTP request context from Om context
-- |
-- | Example:
-- |   req <- httpRequest
-- |   let hdrs = req.headers
httpRequest
  :: forall err rest
   . Om { httpRequest :: RequestContext | rest } err RequestContext
httpRequest = Om.asks _.httpRequest

-- | Get headers from Om context (no parameter needed!)
-- |
-- | Example:
-- |   hdrs <- requestHeaders
-- |   case Object.lookup "authorization" hdrs of ...
requestHeaders
  :: forall err rest
   . Om { httpRequest :: RequestContext | rest } err (Object String)
requestHeaders = Om.asks _.httpRequest.headers

-- | Get path params from Om context (no parameter needed!)
-- |
-- | Example:
-- |   ps <- requestParams
-- |   case Object.lookup "id" ps of ...
requestParams
  :: forall err rest
   . Om { httpRequest :: RequestContext | rest } err (Object String)
requestParams = Om.asks _.httpRequest.params

-- | Get query params from Om context (no parameter needed!)
-- |
-- | Example:
-- |   q <- requestQuery
-- |   case Object.lookup "limit" q of ...
requestQuery
  :: forall err rest
   . Om { httpRequest :: RequestContext | rest } err (Object Foreign)
requestQuery = Om.asks _.httpRequest.query

-- | Get request body from Om context (no parameter needed!)
-- |
-- | Example:
-- |   bodyMaybe <- requestBody
-- |   case bodyMaybe of ...
requestBody
  :: forall err rest
   . Om { httpRequest :: RequestContext | rest } err (Maybe Foreign)
requestBody = Om.asks _.httpRequest.body

-- | Get HTTP method from Om context (no parameter needed!)
requestMethod
  :: forall err rest
   . Om { httpRequest :: RequestContext | rest } err HTTPMethod
requestMethod = Om.asks _.httpRequest.method

-- | Get request URL from Om context (no parameter needed!)
requestUrl
  :: forall err rest
   . Om { httpRequest :: RequestContext | rest } err RouteURL
requestUrl = Om.asks _.httpRequest.url

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
  :: forall rest
   . String
  -> Om { httpRequest :: RequestContext | rest } (missingParam :: String) String
requiredParam key = do
  ps <- requestParams
  case Object.lookup key ps of
    Just value -> pure value
    Nothing -> Om.throw { missingParam: key }

-- | Get an optional path parameter
-- |
-- | Example:
-- |   userIdMaybe <- param "id"
param
  :: forall err rest
   . String
  -> Om { httpRequest :: RequestContext | rest } err (Maybe String)
param key = do
  ps <- requestParams
  pure $ Object.lookup key ps

-- | Parse a String to a specific type (used internally by requiredParams)
class ParseParam a where
  parseParam :: String -> Either String a

instance ParseParam String where
  parseParam = Right

instance ParseParam Int where
  parseParam s = case Int.fromString s of
    Nothing -> Left $ "Expected an integer but got: " <> s
    Just n -> Right n

instance ParseParam Number where
  parseParam s = case Int.fromString s of
    Nothing -> Left $ "Expected a number but got: " <> s
    Just n -> Right (Int.toNumber n)

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
  parseParams _ ps = do
    let
      key = Proxy :: Proxy name
      keyName = reflectSymbol key
      -- Check if field exists
      valueResult = case Object.lookup keyName ps of
        Nothing -> Left { missing: [ keyName ], invalid: [] }
        Just valueStr -> case parseParam valueStr of
          Left _ -> Left { missing: [], invalid: [ keyName ] }
          Right value -> Right value
      -- Parse the rest
      restResult = parseParams (Proxy :: Proxy tail) ps
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
  :: forall rest r rl
   . RowToList r rl
  => ParseParams rl r
  => Proxy r
  -> Om { httpRequest :: RequestContext | rest } (paramErrors :: ParamErrors) (Record r)
requiredParams _ = do
  ps <- requestParams
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
  parseOptionalParams _ ps = do
    let
      key = Proxy :: Proxy name
      keyName = reflectSymbol key
      value = Object.lookup keyName ps >>= \v -> case parseParam v of
        Left _ -> Nothing
        Right parsed -> Just parsed
      rest = parseOptionalParams (Proxy :: Proxy tail) ps
    Record.insert key value rest

-- | Get multiple optional path parameters as a typed record with Maybe fields
-- |
-- | Example:
-- |   { userId, postId } <- optionalParams (Proxy :: _ { userId :: Int, postId :: String })
-- |   -- Returns { userId :: Maybe Int, postId :: Maybe String }
optionalParams
  :: forall rest r rl
   . RowToList r rl
  => ParseOptionalParams rl r
  => Proxy r
  -> Om { httpRequest :: RequestContext | rest } () (Record r)
optionalParams _ = do
  ps <- requestParams
  pure $ parseOptionalParams (Proxy :: Proxy rl) ps

-- | Get a required header, throws MissingHeader if not found
-- |
-- | Example:
-- |   authToken <- requiredHeader "authorization"
requiredHeader
  :: forall rest
   . String
  -> Om { httpRequest :: RequestContext | rest } (missingHeader :: String) String
requiredHeader key = do
  hdrs <- requestHeaders
  case Object.lookup key hdrs of
    Just value -> pure value
    Nothing -> Om.throw { missingHeader: key }

-- | Get an optional header
-- |
-- | Example:
-- |   authTokenMaybe <- requestHeader "authorization"
requestHeader
  :: forall err rest
   . String
  -> Om { httpRequest :: RequestContext | rest } err (Maybe String)
requestHeader key = do
  hdrs <- requestHeaders
  pure $ Object.lookup key hdrs

-- | Get multiple required headers as a typed record
-- | Throws HeaderErrors with all missing and invalid fields
-- |
-- | Example:
-- |   { authorization, contentType } <- requiredHeaders (Proxy :: _ { authorization :: String, contentType :: String })
requiredHeaders
  :: forall rest r rl
   . RowToList r rl
  => ParseParams rl r
  => Proxy r
  -> Om { httpRequest :: RequestContext | rest } (headerErrors :: HeaderErrors) (Record r)
requiredHeaders _ = do
  hdrs <- requestHeaders
  case parseParams (Proxy :: Proxy rl) hdrs of
    Right parsed -> pure parsed
    Left errors -> Om.throw { headerErrors: errors }

-- | Get multiple optional headers as a typed record with Maybe fields
-- |
-- | Example:
-- |   { authorization, userAgent } <- optionalHeaders (Proxy :: _ { authorization :: String, userAgent :: String })
-- |   -- Returns { authorization :: Maybe String, userAgent :: Maybe String }
optionalHeaders
  :: forall rest r rl
   . RowToList r rl
  => ParseOptionalParams rl r
  => Proxy r
  -> Om { httpRequest :: RequestContext | rest } () (Record r)
optionalHeaders _ = do
  hdrs <- requestHeaders
  pure $ parseOptionalParams (Proxy :: Proxy rl) hdrs

-- | Get a required query parameter, throws MissingQueryParam if not found
-- | Returns Foreign - use yoga-json to decode
-- |
-- | Example:
-- |   limitForeign <- requiredQueryParam "limit"
-- |   limit <- liftEffect $ J.readJSON limitForeign
requiredQueryParam
  :: forall rest
   . String
  -> Om { httpRequest :: RequestContext | rest } (missingQueryParam :: String) Foreign
requiredQueryParam key = do
  q <- requestQuery
  case Object.lookup key q of
    Just value -> pure value
    Nothing -> Om.throw { missingQueryParam: key }

-- | Get an optional query parameter
-- |
-- | Example:
-- |   limitMaybe <- queryParam "limit"
queryParam
  :: forall err rest
   . String
  -> Om { httpRequest :: RequestContext | rest } err (Maybe Foreign)
queryParam key = do
  q <- requestQuery
  pure $ Object.lookup key q

-- | Typeclass to parse query params (Foreign) into a record, accumulating all errors
-- |
-- | Query params are typed as `Object Foreign` to match TypeScript's `unknown` type.
-- | We use yoga-json's ReadForeign to safely decode Foreign -> String, then ParseParam
-- | to convert String -> target type.
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
  parseQueryParams _ qps = do
    let
      key = Proxy :: Proxy name
      keyName = reflectSymbol key
      -- Check if field exists and parse it (Foreign -> ty via ParseParam)
      valueResult = case Object.lookup keyName qps of
        Nothing -> Left { missing: [ keyName ], invalid: [] }
        Just foreignVal -> case runExcept (readImpl foreignVal) of
          Left _ -> Left { missing: [], invalid: [ keyName ] }
          Right (valueStr :: String) -> case parseParam valueStr of
            Left _ -> Left { missing: [], invalid: [ keyName ] }
            Right value -> Right value
      -- Parse the rest
      restResult = parseQueryParams (Proxy :: Proxy tail) qps
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
  :: forall rest r rl
   . RowToList r rl
  => ParseQueryParams rl r
  => Proxy r
  -> Om { httpRequest :: RequestContext | rest } (queryParamErrors :: QueryParamErrors) (Record r)
requiredQueryParams _ = do
  q <- requestQuery
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
  parseOptionalQueryParams _ qps = do
    let
      key = Proxy :: Proxy name
      keyName = reflectSymbol key
      value = do
        foreignVal <- Object.lookup keyName qps
        valueStr <- case runExcept (readImpl foreignVal) of
          Left _ -> Nothing
          Right (s :: String) -> Just s
        case parseParam valueStr of
          Left _ -> Nothing
          Right parsed -> Just parsed
      rest = parseOptionalQueryParams (Proxy :: Proxy tail) qps
    Record.insert key value rest

-- | Get multiple optional query parameters as a typed record with Maybe fields
-- |
-- | Example:
-- |   { page, limit } <- optionalQueryParams (Proxy :: _ { page :: Int, limit :: Int })
-- |   -- Returns { page :: Maybe Int, limit :: Maybe Int }
optionalQueryParams
  :: forall rest r rl
   . RowToList r rl
  => ParseOptionalQueryParams rl r
  => Proxy r
  -> Om { httpRequest :: RequestContext | rest } () (Record r)
optionalQueryParams _ = do
  q <- requestQuery
  pure $ parseOptionalQueryParams (Proxy :: Proxy rl) q
