module Yoga.HTTP.API.Route.RenderMethod
  ( class RenderMethod
  , renderMethod
  ) where

import Type.Proxy (Proxy)
import Yoga.HTTP.API.Route.Method (DELETE, GET, PATCH, POST, PUT)

--------------------------------------------------------------------------------
-- RenderMethod Typeclass
--------------------------------------------------------------------------------

-- | Render HTTP method to lowercase string (OpenAPI format)
class RenderMethod (method :: Type) where
  renderMethod :: Proxy method -> String

--------------------------------------------------------------------------------
-- HTTP Method Instances
--------------------------------------------------------------------------------

instance RenderMethod GET where
  renderMethod _ = "get"

instance RenderMethod POST where
  renderMethod _ = "post"

instance RenderMethod PUT where
  renderMethod _ = "put"

instance RenderMethod DELETE where
  renderMethod _ = "delete"

instance RenderMethod PATCH where
  renderMethod _ = "patch"
