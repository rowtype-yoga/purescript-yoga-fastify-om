module Yoga.HTTP.API.Route.Handler
  ( HandlerFn
  , Request(..)
  , NoRequest
  , class DefaultRequestFields
  , class DefaultRequestFieldsRL
  , class SegmentPathParams
  , class SegmentQueryParams
  , class SegmentQueryParamsRL
  , class EncodingBody
  , class CaptureParams
  , class RequestHeaders
  , class RequestCookies
  , class RequestBody
  ) where

import Data.Maybe (Maybe)
import Data.Unit (Unit)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Prim.Row as Row
import Prim.RowList as RL
import Yoga.HTTP.API.Path (Path, PathCons, Capture, Param, QueryParams, Required)
import Yoga.HTTP.API.Route.Encoding (JSON, NoBody)

--------------------------------------------------------------------------------
-- NoRequest: Type alias for empty request (no headers, cookies, or body)
--------------------------------------------------------------------------------

-- | Type alias for routes with no request data (no headers, cookies, or body).
-- |
-- | Usage:
-- |   Route GET "health" NoRequest (ok :: { body :: String })
-- |
-- | Equivalent to:
-- |   Route GET "health" {} (ok :: { body :: String })
type NoRequest = Record ()

data Request :: Type -> Type
data Request r = Request r
--------------------------------------------------------------------------------
-- Handler Type
--------------------------------------------------------------------------------

-- | Type-safe handler tied to a route's computed types.
-- |
-- | Usage:
-- |   myHandler :: Handler (id :: Int) (limit :: Maybe Int) (authorization :: BearerToken) User
-- |     (ok :: ResponseData () (Array Post), notFound :: ResponseData () ErrorMessage)
-- |   myHandler { path, query, headers, body } = do
-- |     -- path :: { id :: Int }
-- |     -- query :: { limit :: Maybe Int }
-- |     -- headers :: { authorization :: BearerToken }
-- |     -- body :: User
-- |     pure $ respondNoHeaders (Proxy :: _ "ok") []
type HandlerFn pathParams queryParams reqHeaders body respVariant =
  { path :: Record pathParams
  , query :: Record queryParams
  , headers :: Record reqHeaders
  , body :: body
  }
  -> Aff (Variant respVariant)

--------------------------------------------------------------------------------
-- SegmentPathParams: Extract path capture row from segments
--------------------------------------------------------------------------------

-- | Extract the row of typed path parameters from a path segments type.
class SegmentPathParams :: forall k. k -> Row Type -> Constraint
class SegmentPathParams segments params | segments -> params

instance segmentPathParamsQueryParams ::
  CaptureParams path params =>
  SegmentPathParams (QueryParams path (Record q)) params

else instance segmentPathParamsCatchAll ::
  CaptureParams segs params =>
  SegmentPathParams segs params

--------------------------------------------------------------------------------
-- SegmentQueryParams: Extract query param row from segments
--------------------------------------------------------------------------------

-- | Extract the row of typed query parameters from a path segments type.
class SegmentQueryParams :: forall k. k -> Row Type -> Constraint
class SegmentQueryParams segments query | segments -> query

instance segmentQueryParamsQP ::
  ( RL.RowToList params rl
  , SegmentQueryParamsRL rl query
  ) =>
  SegmentQueryParams (QueryParams path (Record params)) query

else instance segmentQueryParamsCatchAll :: SegmentQueryParams segs ()

-- | RowList-based processing of query param rows.
-- | Required ty → ty (plain), otherwise → Maybe ty
class SegmentQueryParamsRL (rl :: RL.RowList Type) (query :: Row Type) | rl -> query

instance segmentQueryParamsRLNil :: SegmentQueryParamsRL RL.Nil ()

instance segmentQueryParamsRLConsRequired ::
  ( SegmentQueryParamsRL tail tailQuery
  , Row.Cons name ty tailQuery query
  , Row.Lacks name tailQuery
  ) =>
  SegmentQueryParamsRL (RL.Cons name (Required ty) tail) query

else instance segmentQueryParamsRLConsOptional ::
  ( SegmentQueryParamsRL tail tailQuery
  , Row.Cons name (Maybe ty) tailQuery query
  , Row.Lacks name tailQuery
  ) =>
  SegmentQueryParamsRL (RL.Cons name ty tail) query

--------------------------------------------------------------------------------
-- CaptureParams: Extract captures from path segments
--------------------------------------------------------------------------------

-- | Walk the path segments and collect all Capture/Param entries into a row.
class CaptureParams :: forall k. k -> Row Type -> Constraint
class CaptureParams segs (params :: Row Type) | segs -> params

-- PathCons: merge left and right (most specific, check first)
instance captureParamsCons ::
  ( CaptureParams left leftParams
  , CaptureParams right rightParams
  , Row.Union leftParams rightParams params
  , Row.Nub params params
  ) =>
  CaptureParams (PathCons left right) params

-- Capture: one param
else instance captureParamsCapture ::
  ( Row.Cons name ty () params
  , Row.Lacks name ()
  ) =>
  CaptureParams (Capture name ty) params

-- Param sugar: one param
else instance captureParamsParam ::
  ( Row.Cons name ty () params
  , Row.Lacks name ()
  ) =>
  CaptureParams (Param name ty) params

-- Path wrapper: unwrap and delegate
else instance captureParamsPath :: CaptureParams segs params => CaptureParams (Path segs) params

-- Catch-all for literals (Symbol, Lit, Root, etc.): no params
else instance captureParamsDefault :: CaptureParams s ()

--------------------------------------------------------------------------------
-- EncodingBody: Unwrap encoding phantom type to the actual body type
--------------------------------------------------------------------------------

-- | Map encoding phantom types to the runtime body type the handler receives.
class EncodingBody (encoding :: Type) (body :: Type) | encoding -> body

instance encodingBodyJSON :: EncodingBody (JSON a) a
instance encodingBodyNoBody :: EncodingBody NoBody Unit
instance EncodingBody (Record row) (Record row)

--------------------------------------------------------------------------------
-- RequestHeaders: Extract headers row from a request type
--------------------------------------------------------------------------------

-- | Extract the headers row from a request record type.
-- |
-- | The request is expected to have a `headers` field.
class RequestHeaders (request :: Type) (headers :: Row Type) | request -> headers

instance requestHeadersRequest ::
  ( Row.Cons "headers" (Record headers) _rest requestRow
  ) =>
  RequestHeaders (Record requestRow) headers

--------------------------------------------------------------------------------
-- RequestCookies: Extract cookies row from a request type
--------------------------------------------------------------------------------

-- | Extract the cookies row from a request record type.
-- |
-- | The request is expected to have a `cookies` field.
class RequestCookies (request :: Type) (cookies :: Row Type) | request -> cookies

instance requestCookiesRequest ::
  ( Row.Cons "cookies" (Record cookies) _rest requestRow
  ) =>
  RequestCookies (Record requestRow) cookies

--------------------------------------------------------------------------------
-- RequestBody: Extract body encoding from a request type
--------------------------------------------------------------------------------

-- | Extract the body encoding type from a request record type.
-- |
-- | The request is expected to have a `body` field.
class RequestBody (request :: Type) (encoding :: Type) | request -> encoding

instance requestBodyRequest ::
  ( Row.Cons "body" encoding _rest requestRow
  ) =>
  RequestBody (Record requestRow) encoding

--------------------------------------------------------------------------------
-- DefaultRequestFields: Compute defaults for missing request fields
--------------------------------------------------------------------------------

-- | Compute defaults for missing request fields using RowList.
-- |
-- | When a Request omits `headers`, defaults to `()`.
-- | When a Request omits `cookies`, defaults to `()`.
-- | When a Request omits `body`, defaults to `NoBody`.
class DefaultRequestFields (partialRequest :: Row Type) (fullHeaders :: Row Type) (fullCookies :: Row Type) (fullEncoding :: Type) | partialRequest -> fullHeaders fullCookies fullEncoding

instance defaultRequestFieldsImpl ::
  ( RL.RowToList partialRequest rl
  , DefaultRequestFieldsRL rl partialRequest fullHeaders fullCookies fullEncoding
  ) =>
  DefaultRequestFields partialRequest fullHeaders fullCookies fullEncoding

-- | RowList-based implementation
class DefaultRequestFieldsRL (rl :: RL.RowList Type) (partialRequest :: Row Type) (fullHeaders :: Row Type) (fullCookies :: Row Type) (fullEncoding :: Type) | rl partialRequest -> fullHeaders fullCookies fullEncoding

-- Empty list - no fields present, default all
instance defaultFieldsRLNil :: DefaultRequestFieldsRL RL.Nil partialRequest () () NoBody

-- headers present
instance defaultFieldsRLHeadersCons ::
  ( DefaultRequestFieldsRL tail partialRequest () cookies encoding
  ) =>
  DefaultRequestFieldsRL (RL.Cons "headers" (Record headers) tail) partialRequest headers cookies encoding

-- cookies present
else instance defaultFieldsRLCookiesCons ::
  ( DefaultRequestFieldsRL tail partialRequest headers () encoding
  ) =>
  DefaultRequestFieldsRL (RL.Cons "cookies" (Record cookies) tail) partialRequest headers cookies encoding

-- body present
else instance defaultFieldsRLBodyCons ::
  ( DefaultRequestFieldsRL tail partialRequest headers cookies NoBody
  ) =>
  DefaultRequestFieldsRL (RL.Cons "body" encoding tail) partialRequest headers cookies encoding

-- Other fields (catch-all for unrecognized fields)
else instance defaultFieldsRLOtherCons ::
  ( DefaultRequestFieldsRL tail partialRequest headers cookies encoding
  ) =>
  DefaultRequestFieldsRL (RL.Cons otherLabel otherType tail) partialRequest headers cookies encoding
