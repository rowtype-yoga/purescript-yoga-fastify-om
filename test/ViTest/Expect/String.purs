module ViTest.Expect.String where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CodePoints (indexOf)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

expectContains :: String -> String -> Aff Unit
expectContains needle haystack =
  case indexOf (Pattern needle) haystack of
    Just _ -> pure unit
    Nothing -> fail message
  where
  message = "Expected compiler output to contain:\n"
    <> needle
    <> "\n\nActual compiler output:\n"
    <> haystack

fail :: String -> Aff Unit
fail = liftEffect <<< failImpl

foreign import failImpl :: String -> Effect Unit
