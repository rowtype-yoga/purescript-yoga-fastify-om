module ViTest.Expect.Either where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import ViTest.Expect (expectToBe)

expectRight :: forall e a. Eq a => Show e => a -> Either e a -> Aff Unit
expectRight expected result = case result of
  Right actual -> expectToBe true (expected == actual)
  Left _ -> expectToBe true false

expectIsRight :: forall e a. Show e => Either e a -> Aff Unit
expectIsRight result = case result of
  Right _ -> expectToBe true true
  Left _ -> expectToBe true false

expectLeft :: forall e a. Eq e => Show a => e -> Either e a -> Aff Unit
expectLeft expected result = case result of
  Left actual -> expectToBe true (expected == actual)
  Right _ -> expectToBe true false

expectIsLeft :: forall e a. Show a => Either e a -> Aff Unit
expectIsLeft result = case result of
  Left _ -> expectToBe true true
  Right _ -> expectToBe true false

expectLeftContains :: forall a. Show a => String -> Either String a -> Aff Unit
expectLeftContains needle result = case result of
  Left err ->
    if contains needle err
    then expectToBe true true
    else expectToBe true false
  Right _ -> expectToBe true false
  where
  contains :: String -> String -> Boolean
  contains n h = indexOf n h >= 0

  indexOf :: String -> String -> Int
  indexOf = indexOfImpl

foreign import indexOfImpl :: String -> String -> Int
