module Yoga.Fastify.Om.Route
  ( -- * HTTP Method Types
    GET
  , POST
  , PUT
  , DELETE
  , PATCH
  -- * Route Type Classes (commented out - invalid instance heads in 0.15)
  -- , class ExtractPath
  -- , extractPath
  -- , class ExtractMethod
  -- , extractMethod
  -- , class ExtractRequest
  -- , extractRequest
  -- , class ExtractResponse
  -- , extractResponse
  -- , class ExtractErrors
  -- , extractErrors
  -- * Handler Type (Phase 3)
  , Handler
  -- , class PathParamsFor  -- TODO: Export when PathParams is ready
  -- , pathParamsFor
  ) where

import Prelude

import Type.Proxy (Proxy(..))

-- import Yoga.Fastify.Om.Path (class PathParams)  -- TODO: Uncomment when PathParams is exported

--------------------------------------------------------------------------------
-- HTTP Method Types
--------------------------------------------------------------------------------

-- | HTTP GET method
data GET

-- | HTTP POST method
data POST

-- | HTTP PUT method
data PUT

-- | HTTP DELETE method
data DELETE

-- | HTTP PATCH method
data PATCH

--------------------------------------------------------------------------------
-- Route Component Extraction Type Classes
--------------------------------------------------------------------------------

-- -- | Extract the path type from a route
-- -- |
-- -- | Example:
-- -- |   ExtractPath { path :: Path @"users", ... } (Path @"users")
-- class ExtractPath (route :: Type) (path :: Type) | route -> path
--
-- instance extractPathFromRecord :: ExtractPath { path :: path } path
--
-- -- Helper function (mainly for testing/debugging)
-- extractPath :: forall route path. ExtractPath route path => Proxy path
-- extractPath = Proxy
--
-- -- | Extract the HTTP method from a route
-- -- |
-- -- | Example:
-- -- |   ExtractMethod { method :: GET, ... } GET
-- class ExtractMethod (route :: Type) (method :: Type) | route -> method
--
-- instance extractMethodFromRecord ::
--   ExtractMethod { method :: method } method
--
-- -- Helper function
-- extractMethod :: forall route method. ExtractMethod route method => Proxy method
-- extractMethod = Proxy
--
-- -- | Extract the request type from a route
-- -- |
-- -- | Example:
-- -- |   ExtractRequest { request :: { body :: User }, ... } { body :: User }
-- class ExtractRequest (route :: Type) (request :: Type) | route -> request
--
-- instance extractRequestFromRecord ::
--   ExtractRequest { request :: request } request
--
-- -- Helper function
-- extractRequest :: forall route request. ExtractRequest route request => Proxy request
-- extractRequest = Proxy
--
-- -- | Extract the response type from a route
-- -- |
-- -- | Example:
-- -- |   ExtractResponse { response :: { body :: User }, ... } { body :: User }
-- class ExtractResponse (route :: Type) (response :: Type) | route -> response
--
-- instance extractResponseFromRecord ::
--   ExtractResponse { response :: response } response
--
-- -- Helper function
-- extractResponse :: forall route response. ExtractResponse route response => Proxy response
-- extractResponse = Proxy
--
-- -- | Extract the errors row from a route
-- -- |
-- -- | Example:
-- -- |   ExtractErrors { errors :: (notFound :: _, unauthorized :: _), ... }
-- -- |                 (notFound :: _, unauthorized :: _)
-- class ExtractErrors (route :: Type) (errors :: Row Type) | route -> errors
--
-- instance extractErrorsFromRecord ::
--   ExtractErrors { errors :: errors } errors
--
-- -- Helper function
-- extractErrors :: forall route errors. ExtractErrors route errors => Proxy (Record errors)
-- extractErrors = Proxy
--
--------------------------------------------------------------------------------
-- Handler Type (Phase 3)
--------------------------------------------------------------------------------
--
-- -- | Extract path parameters from a route's path
-- -- |
-- -- | This combines ExtractPath and PathParams to get the parameter row from a route
-- class PathParamsFor (route :: Type) (params :: Row Type) | route -> params
--
-- instance pathParamsForRoute ::
--   ( ExtractPath route path
--   , PathParams path params
--   ) =>
--   PathParamsFor route params
--
-- -- Helper function
-- pathParamsFor :: forall route params. PathParamsFor route params => Proxy (Record params)
-- pathParamsFor = Proxy

-- | Handler type - tied to a specific route via type parameter
-- |
-- | For now (Phase 3), we keep it simple:
-- | - No Om monad yet (just a placeholder type)
-- | - No error handling yet
-- | - Just demonstrates the type structure
-- |
-- | The handler receives:
-- | - path: Record of typed path parameters (extracted from route's path type)
-- | - request: The request type from the route
-- |
-- | And returns the response type from the route.
-- |
-- | Example:
-- |   handler :: Handler GetUserRoute {}
-- |   handler { path, request } = ...
-- |     where path :: { id :: Int }  -- from Capture "id" Int
-- |           request :: {}           -- from route's request field
-- |
-- | Phase 4 will add Om monad with error handling.
-- TODO: Re-enable path params when PathParams is ready
-- TODO: Re-enable extraction classes when valid in PureScript 0.15
type Handler route ctx =
  -- forall request response errors
  -- . PathParamsFor route pathParams  -- TODO: Uncomment when ready
  -- . ExtractRequest route request
  -- => ExtractResponse route response
  -- => ExtractErrors route errors
  -- =>
  { path :: {} -- TODO: Will be Record pathParams when PathParams is ready
  , request :: {} -- TODO: Will be request type when extraction classes work
  }
  -> {} -- Simplified for Phase 3, will be typed response and Om monad in Phase 4
