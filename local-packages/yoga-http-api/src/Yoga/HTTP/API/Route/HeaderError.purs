module Yoga.HTTP.API.Route.HeaderError
  ( HeaderError(..)
  ) where

import Prelude

--------------------------------------------------------------------------------
-- Header Errors
--------------------------------------------------------------------------------

-- | Typed errors for header parsing
data HeaderError
  = MissingHeader String
  | InvalidHeaderValue String String -- header name, error message

derive instance Eq HeaderError

instance Show HeaderError where
  show (MissingHeader name) = "Missing required header: " <> name
  show (InvalidHeaderValue name msg) = "Invalid header '" <> name <> "': " <> msg
