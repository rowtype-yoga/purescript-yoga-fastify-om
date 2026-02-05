module Yoga.Fastify.Om.Route.HandleResponse
  ( class HandleResponse
  , handleResponse
  , class HandleResponseRL
  , handleResponseRL
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Fastify (FastifyReply)
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Om.Route.Response (ResponseData(..))
import Yoga.Fastify.Om.Route.SetHeaders (class SetHeaders, setHeaders)
import Yoga.Fastify.Om.Route.StatusCode (class StatusCodeMap, statusCodeFor)
import Yoga.JSON (class WriteForeign, writeImpl)

class HandleResponse (respVariant :: Row Type) where
  handleResponse :: Proxy respVariant -> Variant respVariant -> FastifyReply -> Aff Unit

instance (RowToList respVariant rl, HandleResponseRL rl respVariant) => HandleResponse respVariant where
  handleResponse _ = handleResponseRL (Proxy :: Proxy rl)

class HandleResponseRL (rl :: RowList Type) (respVariant :: Row Type) | rl -> respVariant where
  handleResponseRL :: Proxy rl -> Variant respVariant -> FastifyReply -> Aff Unit

instance HandleResponseRL RL.Nil () where
  handleResponseRL _ = Variant.case_

instance
  ( IsSymbol label
  , StatusCodeMap label
  , SetHeaders headers
  , WriteForeign body
  , HandleResponseRL tail rest
  , Cons label (ResponseData headers body) rest respVariant
  , Lacks label rest
  ) =>
  HandleResponseRL (RL.Cons label (ResponseData headers body) tail) respVariant where
  handleResponseRL _ variant reply =
    Variant.on (Proxy :: Proxy label) handler rest variant
    where
    handler :: ResponseData headers body -> Aff Unit
    handler (ResponseData rd) = do
      let statusCode = statusCodeFor (Proxy :: Proxy label)
      void $ liftEffect $ F.status statusCode reply
      void $ liftEffect $ setHeaders rd.headers reply
      let encoded = writeImpl rd.body
      F.send (unsafeToForeign encoded) reply

    rest :: Variant rest -> Aff Unit
    rest v = handleResponseRL (Proxy :: Proxy tail) v reply
