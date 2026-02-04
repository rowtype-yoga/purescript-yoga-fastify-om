module Yoga.Fastify.Om.Router
  ( -- * Router Integration
    matchRoute
  , matchRouteUrl
  , getRoute
  -- * Re-exports from routing-duplex
  , module Routing.Duplex
  , module Routing.Duplex.Generic
  , module Routing.Duplex.Generic.Syntax
  ) where

import Prelude

import Data.Either (Either)
import Prim.Row (class Cons)
import Routing.Duplex (RouteDuplex', parse, print)
import Routing.Duplex as Routing.Duplex
import Routing.Duplex.Generic as Routing.Duplex.Generic
import Routing.Duplex.Generic.Syntax as Routing.Duplex.Generic.Syntax
import Routing.Duplex.Parser (RouteError)
import Yoga.Fastify.Fastify (RouteURL(..))
import Yoga.Fastify.Om as FO
import Yoga.Om as Om

-- | Match the current request URL against a route codec
-- |
-- | Example:
-- |   handler reply = do
-- |     route <- matchRoute myRouteCodec
-- |     case route of
-- |       Right (UserProfile userId) -> showUserProfile userId reply
-- |       Right Home -> showHome reply
-- |       Left err -> badRequest reply
matchRoute
  :: forall route ctx err
   . RouteDuplex' route
  -> Om.Om { httpRequest :: FO.RequestContext | ctx } err (Either RouteError route)
matchRoute codec = do
  url <- FO.requestUrl
  pure $ parse codec (unwrapUrl url)
  where
  unwrapUrl (RouteURL s) = s

-- | Match a specific RouteURL against a route codec (for testing/utilities)
matchRouteUrl
  :: forall route
   . RouteDuplex' route
  -> RouteURL
  -> Either RouteError route
matchRouteUrl codec (RouteURL url) = parse codec url

-- | Get the current route from context (convenience wrapper around matchRoute)
-- |
-- | Example:
-- |   handler reply = do
-- |     route <- getRoute myRouteCodec
-- |     case route of
-- |       Right (UserProfile userId) -> showProfile userId reply
-- |       Right Home -> showHome reply
-- |       Left err -> badRequest reply
getRoute
  :: forall route ctx err
   . RouteDuplex' route
  -> Om.Om { httpRequest :: FO.RequestContext | ctx } err (Either RouteError route)
getRoute = matchRoute
