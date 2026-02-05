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
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Fastify (Fastify, FastifyReply, FastifyRequest, HTTPMethod(..), RouteURL(..), StatusCode(..))
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Om.Path (class PathPattern, pathPattern)
import Yoga.Fastify.Om.Route.Route (Route)
import Yoga.Fastify.Om.Route.HandleResponse (class HandleResponse, handleResponse)
import Yoga.Fastify.Om.Route.Handler (Handler, class DefaultRequestFields, class SegmentPathParams, class SegmentQueryParams, class EncodingBody)
import Yoga.Fastify.Om.Route.ParseBody (class ParseBody, parseBody)
import Yoga.Fastify.Om.Route.ParseHeaders (class ParseHeaders, parseHeaders)
import Yoga.Fastify.Om.Route.ParsePathParams (class ParsePathParams, parsePathParams)
import Yoga.Fastify.Om.Route.ParseQueryParams (class ParseQueryParamsFromObject, parseQueryParamsFromObject)
import Yoga.Fastify.Om.Route.RenderMethod (class RenderMethod, renderMethod)
import Yoga.JSON (writeJSON)

handleRoute
  :: forall method segments partialRequest o_ fullHeaders fullEncoding respVariant
       pathParams queryParams body
   . Row.Union partialRequest o_ (headers :: Record fullHeaders, body :: fullEncoding)
  => DefaultRequestFields partialRequest fullHeaders fullEncoding
  => RenderMethod method
  => PathPattern segments
  => SegmentPathParams segments pathParams
  => SegmentQueryParams segments queryParams
  => EncodingBody fullEncoding body
  => ParsePathParams pathParams
  => ParseQueryParamsFromObject queryParams
  => ParseHeaders fullHeaders
  => ParseBody fullEncoding body
  => HandleResponse respVariant
  => Proxy (Route method segments (Record partialRequest) respVariant)
  -> Handler pathParams queryParams fullHeaders body respVariant
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
            case parseHeaders (Proxy :: Proxy fullHeaders) headersObj of
              Left errs -> send400 reply (writeJSON { error: "Invalid request headers", details: map show (NEA.toArray errs) })
              Right headers ->
                case parseBody (Proxy :: Proxy fullEncoding) bodyMaybe of
                  Left err -> send400 reply (writeJSON { error: "Invalid request body", details: [ err ] })
                  Right body -> do
                    result <- handler { path, query, headers, body }
                    handleResponse (Proxy :: Proxy respVariant) result reply

  send400 :: FastifyReply -> String -> _
  send400 reply errorJson = do
    void $ liftEffect $ F.status (StatusCode 400) reply
    void $ liftEffect $ F.header "content-type" "application/json" reply
    F.send (unsafeToForeign errorJson) reply
