module Yoga.Fastify.Om.Route.HandleRoute
  ( handleRoute
  ) where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.String as String
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Fastify (Fastify, FastifyReply, FastifyRequest, HTTPMethod(..), RouteURL(..), StatusCode(..))
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Om.Path (class PathPattern, pathPattern)
import Yoga.Fastify.Om.Route.Route (Route)
import Yoga.Fastify.Om.Route.HandleResponse (class HandleResponse, handleResponse)
import Yoga.Fastify.Om.Route.Handler (Handler, class SegmentPathParams, class SegmentQueryParams, class EncodingBody, class RequestHeaders, class RequestBody)
import Yoga.Fastify.Om.Route.ParseBody (class ParseBody, parseBody)
import Yoga.Fastify.Om.Route.ParseHeaders (class ParseHeaders, parseHeaders)
import Yoga.Fastify.Om.Route.ParsePathParams (class ParsePathParams, parsePathParams)
import Yoga.Fastify.Om.Route.ParseQueryParams (class ParseQueryParamsFromObject, parseQueryParamsFromObject)
import Yoga.Fastify.Om.Route.RenderMethod (class RenderMethod, renderMethod)
import Yoga.JSON (writeJSON)

handleRoute
  :: forall method segments request respVariant
       pathParams queryParams reqHeaders encoding body
   . RenderMethod method
  => PathPattern segments
  => SegmentPathParams segments pathParams
  => SegmentQueryParams segments queryParams
  => RequestHeaders request reqHeaders
  => RequestBody request encoding
  => EncodingBody encoding body
  => ParsePathParams pathParams
  => ParseQueryParamsFromObject queryParams
  => ParseHeaders reqHeaders
  => ParseBody encoding body
  => HandleResponse respVariant
  => Proxy (Route method segments request respVariant)
  -> Handler pathParams queryParams reqHeaders body respVariant
  -> Fastify
  -> Effect Unit
handleRoute _ handler fastify =
  F.route
    { method: HTTPMethod (String.toUpper (renderMethod (Proxy :: Proxy method)))
    , url: RouteURL (pathPattern (Proxy :: Proxy segments))
    }
    routeHandler
    fastify
  where
  routeHandler :: FastifyRequest -> FastifyReply -> _
  routeHandler req reply = do
    paramsObj <- liftEffect $ F.params req
    queryObj <- liftEffect $ F.query req
    headersObj <- liftEffect $ F.headers req
    bodyMaybe <- liftEffect $ F.body req

    case parsePathParams (Proxy :: Proxy pathParams) paramsObj of
      Left errs -> send400 reply (writeJSON { error: "Invalid path parameters", details: errs })
      Right path ->
        case parseQueryParamsFromObject (Proxy :: Proxy queryParams) queryObj of
          Left errs -> send400 reply (writeJSON { error: "Invalid query parameters", details: errs })
          Right query ->
            case parseHeaders (Proxy :: Proxy reqHeaders) headersObj of
              Left errs -> send400 reply (writeJSON { error: "Invalid request headers", details: map show (NEA.toArray errs) })
              Right requestHeaders ->
                case parseBody (Proxy :: Proxy encoding) bodyMaybe of
                  Left err -> send400 reply (writeJSON { error: "Invalid request body", details: [ err ] })
                  Right requestBody -> do
                    result <- handler { path, query, requestHeaders, requestBody }
                    handleResponse (Proxy :: Proxy respVariant) result reply

  send400 :: FastifyReply -> String -> _
  send400 reply errorJson = do
    void $ liftEffect $ F.status (StatusCode 400) reply
    void $ liftEffect $ F.header "content-type" "application/json" reply
    F.send (unsafeToForeign errorJson) reply
