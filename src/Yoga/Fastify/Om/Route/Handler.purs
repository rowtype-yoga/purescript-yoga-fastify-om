module Yoga.Fastify.Om.Route.Handler
  ( Handler
  , class SegmentPathParams
  , class SegmentQueryParams
  , class SegmentQueryParamsRL
  , class EncodingBody
  , class CaptureParams
  , class RequestHeaders
  , class RequestBody
  ) where

import Data.Maybe (Maybe)
import Data.Unit (Unit)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Prim.Row as Row
import Prim.RowList as RL
import Yoga.Fastify.Om.Path (Path, PathCons, Capture, Param, QueryParams, Required)
import Yoga.Fastify.Om.Route.Encoding (JSON, NoBody)

--------------------------------------------------------------------------------
-- Handler Type
--------------------------------------------------------------------------------

-- | Type-safe handler tied to a route's computed types.
-- |
-- | Usage:
-- |   myHandler :: Handler (id :: Int) (limit :: Maybe Int) (authorization :: BearerToken) User
-- |     (ok :: ResponseData () (Array Post), notFound :: ResponseData () ErrorMessage)
-- |   myHandler { path, query, requestHeaders, requestBody } = do
-- |     -- path :: { id :: Int }
-- |     -- query :: { limit :: Maybe Int }
-- |     -- requestHeaders :: { authorization :: BearerToken }
-- |     -- requestBody :: User
-- |     pure $ respondNoHeaders (Proxy :: _ "ok") []
-- |
-- | At a route registration site, constraints tie these to the Route type:
-- |   SegmentPathParams segments pathParams =>
-- |   SegmentQueryParams segments queryParams =>
-- |   EncodingBody reqBody body =>
-- |   Handler pathParams queryParams reqHeaders body respVariant
type Handler pathParams queryParams reqHeaders body respVariant =
  { path :: Record pathParams
  , query :: Record queryParams
  , requestHeaders :: Record reqHeaders
  , requestBody :: body
  }
  -> Aff (Variant respVariant)

--------------------------------------------------------------------------------
-- SegmentPathParams: Extract path capture row from segments
--------------------------------------------------------------------------------

-- | Extract the row of typed path parameters from a path segments type.
class SegmentPathParams (segments :: Type) (params :: Row Type) | segments -> params

instance segmentPathParamsPath ::
  CaptureParams segs params =>
  SegmentPathParams (Path segs) params

instance segmentPathParamsQueryParams ::
  CaptureParams segs params =>
  SegmentPathParams (QueryParams (Path segs) q) params

--------------------------------------------------------------------------------
-- SegmentQueryParams: Extract query param row from segments
--------------------------------------------------------------------------------

-- | Extract the row of typed query parameters from a path segments type.
class SegmentQueryParams (segments :: Type) (query :: Row Type) | segments -> query

instance segmentQueryParamsPath :: SegmentQueryParams (Path segs) ()

instance segmentQueryParamsQP ::
  ( RL.RowToList params rl
  , SegmentQueryParamsRL rl query
  ) =>
  SegmentQueryParams (QueryParams path params) query

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

-- Catch-all for literals (Symbol, Lit, Root, etc.): no params
else instance captureParamsDefault :: CaptureParams s ()

--------------------------------------------------------------------------------
-- EncodingBody: Unwrap encoding phantom type to the actual body type
--------------------------------------------------------------------------------

-- | Map encoding phantom types to the runtime body type the handler receives.
class EncodingBody (encoding :: Type) (body :: Type) | encoding -> body

instance encodingBodyJSON :: EncodingBody (JSON a) a
instance encodingBodyNoBody :: EncodingBody NoBody Unit

--------------------------------------------------------------------------------
-- RequestHeaders: Extract headers row from a request type
--------------------------------------------------------------------------------

-- | Extract the headers row from a request record type.
-- |
-- | The request is expected to be a Record with a `requestHeaders` field.
class RequestHeaders (request :: Type) (headers :: Row Type) | request -> headers

instance requestHeadersRecord ::
  ( Row.Cons "requestHeaders" (Record headers) _rest requestRow
  ) =>
  RequestHeaders (Record requestRow) headers

--------------------------------------------------------------------------------
-- RequestBody: Extract body encoding from a request type
--------------------------------------------------------------------------------

-- | Extract the body encoding type from a request record type.
-- |
-- | The request is expected to be a Record with a `requestBody` field.
class RequestBody (request :: Type) (encoding :: Type) | request -> encoding

instance requestBodyRecord ::
  ( Row.Cons "requestBody" encoding _rest requestRow
  ) =>
  RequestBody (Record requestRow) encoding
