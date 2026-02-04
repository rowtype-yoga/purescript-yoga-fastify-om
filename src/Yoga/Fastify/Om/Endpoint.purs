module Yoga.Fastify.Om.Endpoint
  ( -- * Endpoint Specification
    Endpoint
  , endpoint
  -- * Endpoint Handler
  , EndpointHandler
  , handleEndpoint
  , coerceHandler
  -- * Response Types
  , Response
  , SimpleResponse
  , class SetHeaders
  , setHeaders
  -- * Request Types
  , OptsR
  , OptsOpt
  , Opts
  , EndpointDefaults
  -- * Parsing Typeclasses
  , class ParseRequest
  , parseRequest
  -- * Re-exports
  , module Routing.Duplex
  , module Yoga.Fastify.Om.RequestBody
  , module Yoga.Fastify.Fastify
  ) where

import Prelude

import Control.Monad (void)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Data.Undefined.NoProblem (Opt, fromOpt)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import ForgetMeNot (Id(..))
import Prim.Row (class Cons, class Lacks, class Union, class Nub)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Routing.Duplex (RouteDuplex', parse)
import Routing.Duplex as Routing.Duplex
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Fastify.Fastify (FastifyReply, RouteURL(..), StatusCode(..))
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Om as FO
import Yoga.Fastify.Om.RequestBody (RequestBody(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, read, writeImpl)
import Yoga.Om as Om
import Uncurried.RWSET (withRWSET)

--------------------------------------------------------------------------------
-- Endpoint Specification
--------------------------------------------------------------------------------

-- | Complete typed endpoint specification with record-based request
data Endpoint path request response = Endpoint
  { pathCodec :: RouteDuplex' path
  , requestType :: Proxy request
  , responseType :: Proxy response
  }

-- | Request record parameterized by wrapper type
-- | 
-- | Use with Opt for optional fields, Identity for actual values
type OptsR :: (Type -> Type) -> Type -> Type -> Type -> Type
type OptsR f q h b =
  { query :: f q
  , headers :: f h
  , body :: f b
  }

-- | Request with Opt wrapper (for Union with defaults)
type OptsOpt q h b = OptsR Opt q h b

-- | Request with Identity wrapper (actual runtime values)
type Opts q h b = OptsR Id q h b

-- | Default values for omitted request fields
type EndpointDefaults =
  ( query :: Record ()
  , headers :: Record ()
  , body :: RequestBody Unit
  )

-- | Build an endpoint specification with Union constraint
-- |
-- | Union ensures the request can be merged with defaults to form a valid full request.
-- | At JS runtime: { body: x } === { body: x, query: undefined, headers: undefined }
-- | Use coerceHandler to convert handlers from full to partial types.
endpoint
  :: forall path request o_ query headers body response
   . Union request o_ (query :: query, headers :: headers, body :: body)
  => RouteDuplex' path
  -> Proxy (Record request)
  -> Proxy response
  -> Endpoint path (Record request) response
endpoint pathCodec requestType responseType =
  Endpoint
    { pathCodec
    , requestType
    , responseType
    }

--------------------------------------------------------------------------------
-- Endpoint Handler
--------------------------------------------------------------------------------

-- | Response with status, headers, and body
type Response headers body =
  { status :: StatusCode
  , headers :: Record headers
  , body :: body
  }

-- | Simple response with just body (defaults to 200 OK, no custom headers)
type SimpleResponse body = Response () body

-- | Handler that receives path and fully parsed request record
-- |
-- | Returns a Response with status, headers (homogeneous record), and body
type EndpointHandler ctx err path request response =
  { path :: path
  , request :: request
  }
  -> Om.Om { request :: request, httpRequest :: FO.RequestContext | ctx } err response

--------------------------------------------------------------------------------
-- Parsing Typeclasses
--------------------------------------------------------------------------------

-- | Parse a request record from raw HTTP data using RowList
class ParseRequest (r :: Row Type) where
  parseRequest
    :: Object Foreign -- query params
    -> Object String -- headers
    -> Maybe Foreign -- body
    -> Either String (Record r)

instance
  ( RowToList r rl
  , ParseRequestRL rl r
  ) =>
  ParseRequest r where
  parseRequest = parseRequestRL (Proxy :: Proxy rl)

-- | Helper class that works with RowList
class ParseRequestRL (rl :: RowList Type) (r :: Row Type) | rl -> r where
  parseRequestRL
    :: Proxy rl
    -> Object Foreign
    -> Object String
    -> Maybe Foreign
    -> Either String (Record r)

-- Base case: empty record
instance ParseRequestRL RL.Nil () where
  parseRequestRL _ _ _ _ = Right {}

-- Query field
instance
  ( RowToList queryRow queryRowList
  , ParseQueryFieldRecord queryRowList queryRow
  , ParseRequestRL tail tailRow
  , Lacks "query" tailRow
  ) =>
  ParseRequestRL (RL.Cons "query" (Record queryRow) tail) (query :: Record queryRow | tailRow) where
  parseRequestRL _ queryObj headersObj bodyObj = do
    query <- parseQueryFieldRecord (Proxy :: Proxy queryRowList) queryObj
    rest <- parseRequestRL (Proxy :: Proxy tail) queryObj headersObj bodyObj
    pure $ Record.insert (Proxy :: Proxy "query") query rest

-- Headers field
instance
  ( RowToList headersRow headersRowList
  , ParseHeadersFieldRecord headersRowList headersRow
  , ParseRequestRL tail tailRow
  , Lacks "headers" tailRow
  ) =>
  ParseRequestRL (RL.Cons "headers" (Record headersRow) tail) (headers :: Record headersRow | tailRow) where
  parseRequestRL _ queryObj headersObj bodyObj = do
    headers <- parseHeadersFieldRecord (Proxy :: Proxy headersRowList) headersObj
    rest <- parseRequestRL (Proxy :: Proxy tail) queryObj headersObj bodyObj
    pure $ Record.insert (Proxy :: Proxy "headers") headers rest

-- Body field
instance
  ( ParseBodyField bodyType
  , ParseRequestRL tail tailRow
  , Lacks "body" tailRow
  ) =>
  ParseRequestRL (RL.Cons "body" bodyType tail) (body :: bodyType | tailRow) where
  parseRequestRL _ queryObj headersObj bodyObj = do
    body <- parseBodyField (Proxy :: Proxy bodyType) headersObj bodyObj
    rest <- parseRequestRL (Proxy :: Proxy tail) queryObj headersObj bodyObj
    pure $ Record.insert (Proxy :: Proxy "body") body rest

--------------------------------------------------------------------------------
-- Query Parsing
--------------------------------------------------------------------------------

class ParseQueryFieldRecord (rl :: RowList Type) (r :: Row Type) | rl -> r where
  parseQueryFieldRecord :: Proxy rl -> Object Foreign -> Either String (Record r)

instance ParseQueryFieldRecord RL.Nil () where
  parseQueryFieldRecord _ _ = Right {}

instance
  ( IsSymbol name
  , ParseQueryFieldValue ty
  , ParseQueryFieldRecord tail tailRow
  , Cons name ty tailRow r
  , Lacks name tailRow
  ) =>
  ParseQueryFieldRecord (RL.Cons name ty tail) r where
  parseQueryFieldRecord _ queryObj = do
    let
      key = Proxy :: Proxy name
      keyName = reflectSymbol key
    value <- parseQueryFieldValue (Proxy :: Proxy ty) keyName queryObj
    rest <- parseQueryFieldRecord (Proxy :: Proxy tail) queryObj
    pure $ Record.insert key value rest

class ParseQueryFieldValue (ty :: Type) where
  parseQueryFieldValue :: Proxy ty -> String -> Object Foreign -> Either String ty

instance (FO.ParseParam inner) => ParseQueryFieldValue (Maybe inner) where
  parseQueryFieldValue _ keyName queryObj =
    Right $ case Object.lookup keyName queryObj of
      Nothing -> Nothing
      Just foreignVal ->
        let
          valueStr = unsafeCoerce foreignVal :: String
        in
          FO.parseParam valueStr

else instance (FO.ParseParam ty) => ParseQueryFieldValue ty where
  parseQueryFieldValue _ keyName queryObj =
    case Object.lookup keyName queryObj of
      Nothing -> Left $ "Missing required query parameter: " <> keyName
      Just foreignVal ->
        let
          valueStr = unsafeCoerce foreignVal :: String
        in
          case FO.parseParam valueStr of
            Nothing -> Left $ "Invalid query parameter: " <> keyName
            Just value -> Right value

--------------------------------------------------------------------------------
-- Headers Parsing
--------------------------------------------------------------------------------

class ParseHeadersFieldRecord (rl :: RowList Type) (r :: Row Type) | rl -> r where
  parseHeadersFieldRecord :: Proxy rl -> Object String -> Either String (Record r)

instance ParseHeadersFieldRecord RL.Nil () where
  parseHeadersFieldRecord _ _ = Right {}

instance
  ( IsSymbol name
  , ParseHeaderFieldValue ty
  , ParseHeadersFieldRecord tail tailRow
  , Cons name ty tailRow r
  , Lacks name tailRow
  ) =>
  ParseHeadersFieldRecord (RL.Cons name ty tail) r where
  parseHeadersFieldRecord _ headersObj = do
    let
      key = Proxy :: Proxy name
      keyName = reflectSymbol key
    value <- parseHeaderFieldValue (Proxy :: Proxy ty) keyName headersObj
    rest <- parseHeadersFieldRecord (Proxy :: Proxy tail) headersObj
    pure $ Record.insert key value rest

class ParseHeaderFieldValue (ty :: Type) where
  parseHeaderFieldValue :: Proxy ty -> String -> Object String -> Either String ty

instance ParseHeaderFieldValue (Maybe String) where
  parseHeaderFieldValue _ keyName headersObj =
    Right $ Object.lookup keyName headersObj

else instance ParseHeaderFieldValue String where
  parseHeaderFieldValue _ keyName headersObj =
    case Object.lookup keyName headersObj of
      Nothing -> Left $ "Missing required header: " <> keyName
      Just value -> Right value

--------------------------------------------------------------------------------
-- Body Parsing
--------------------------------------------------------------------------------

-- | Parse body into RequestBody variant (keeps wrapper)
class ParseBodyField (ty :: Type) where
  parseBodyField :: Proxy ty -> Object String -> Maybe Foreign -> Either String ty

-- RequestBody Unit -> NoBody
instance ParseBodyField (RequestBody Unit) where
  parseBodyField _ _ _ = Right NoBody

-- RequestBody a -> JSONBody a (parse via ReadForeign)
else instance ReadForeign a => ParseBodyField (RequestBody a) where
  parseBodyField _ headers bodyMaybe =
    case bodyMaybe of
      Nothing -> Right NoBody
      Just foreignBody ->
        case Object.lookup "content-type" headers of
          Just contentType
            | contentType == "application/json" || contentType == "application/json; charset=utf-8" ->
                case read foreignBody of
                  Left err -> Left $ "Invalid JSON body: " <> show err
                  Right parsed -> Right (JSONBody parsed)
            | otherwise ->
                Left $ "Unsupported Content-Type: " <> contentType
          Nothing ->
            -- Try to parse as JSON by default
            case read foreignBody of
              Left err -> Left $ "Invalid JSON body (no Content-Type header): " <> show err
              Right parsed -> Right (JSONBody parsed)

--------------------------------------------------------------------------------
-- Endpoint Handler Execution
--------------------------------------------------------------------------------

-- | Coerce handler from full request type to partial request type
-- | Safe because at JS runtime: { body: x } === { body: x, query: undefined, headers: undefined }
coerceHandler
  :: forall ctx err path partial full response
   . EndpointHandler ctx err path (Record full) response
  -> EndpointHandler ctx err path (Record partial) response
coerceHandler = unsafeCoerce

--------------------------------------------------------------------------------
-- Response Header Setting
--------------------------------------------------------------------------------

-- | Set response headers from a record using RowList
-- | Field names should be actual HTTP header names (use quotes for special chars)
-- | Example: { "Content-Type": "application/json", "X-Request-Id": "123" }
class SetHeaders (headers :: Row Type) where
  setHeaders :: Record headers -> FastifyReply -> Effect FastifyReply

instance (RowToList headers rl, SetHeadersRL rl headers) => SetHeaders headers where
  setHeaders = setHeadersRL (Proxy :: Proxy rl)

-- | Helper class that works with RowList
class SetHeadersRL (rl :: RowList Type) (headers :: Row Type) | rl -> headers where
  setHeadersRL :: Proxy rl -> Record headers -> FastifyReply -> Effect FastifyReply

instance SetHeadersRL RL.Nil () where
  setHeadersRL _ _ reply = pure reply

instance
  ( IsSymbol name
  , SetHeadersRL tail tailRow
  , Cons name String tailRow headers
  , Lacks name tailRow
  ) =>
  SetHeadersRL (RL.Cons name String tail) headers where
  setHeadersRL _ headers reply = do
    let
      headerName = reflectSymbol (Proxy :: Proxy name)
      value = Record.get (Proxy :: Proxy name) headers
      tailHeaders = Record.delete (Proxy :: Proxy name) headers :: Record tailRow

    reply' <- F.header headerName value reply
    setHeadersRL (Proxy :: Proxy tail) tailHeaders reply'

-- | Execute an endpoint handler with automatic parsing and response sending
handleEndpoint
  :: forall ctx err path request responseRow responseHeaders responseBody o_
   . ParseRequest request
  => Union responseRow o_ (status :: StatusCode, headers :: Record responseHeaders, body :: responseBody)
  => Lacks "request" ctx
  => SetHeaders responseHeaders
  => WriteForeign responseBody
  => Endpoint path (Record request) (Record responseRow)
  -> EndpointHandler ctx err path (Record request) (Record responseRow)
  -> FastifyReply
  -> Om.Om { httpRequest :: FO.RequestContext | ctx } err Unit
handleEndpoint (Endpoint spec) handler reply = do
  -- Get raw request data
  reqCtx <- FO.httpRequest
  let
    queryObj = reqCtx.query
    headersObj = reqCtx.headers
    bodyObj = reqCtx.body

  -- Parse URL path
  url <- FO.requestUrl
  case parse spec.pathCodec (unwrapUrl url) of
    Left _ -> do
      void $ liftEffect $ F.status (StatusCode 404) reply
      void $ FO.send (unsafeToForeign "Not found") reply

    Right path -> do
      -- Parse request record
      case parseRequest queryObj headersObj bodyObj of
        Left parseError -> do
          void $ liftEffect $ F.status (StatusCode 400) reply
          void $ FO.send (unsafeToForeign parseError) reply

        Right request -> do
          response <- withRequestCtx request (handler { path, request })
          let
            responseFull =
              unsafeCoerce response
                :: { status :: Opt StatusCode
                   , headers :: Opt (Record responseHeaders)
                   , body :: responseBody
                   }
            status = fromOpt (StatusCode 200) responseFull.status

            headers :: Record responseHeaders
            headers = fromOpt (unsafeCoerce {}) responseFull.headers
            body = responseFull.body

          -- Set status
          void $ liftEffect $ F.status status reply

          -- Set headers (convert record to HTTP headers)
          void $ liftEffect $ setHeaders headers reply

          -- Use yoga-json to encode response body
          let encodedResponse = writeImpl body
          void $ FO.send encodedResponse reply
  where
  unwrapUrl (RouteURL s) = s

  withRequestCtx
    :: forall ctx' err' request' a
     . Lacks "request" ctx'
    => Record request'
    -> Om.Om { request :: Record request', httpRequest :: FO.RequestContext | ctx' } err' a
    -> Om.Om { httpRequest :: FO.RequestContext | ctx' } err' a
  withRequestCtx req (Om.Om om) =
    Om.Om (withRWSET (\r s -> Record.insert (Proxy :: Proxy "request") req r /\ s) om)
