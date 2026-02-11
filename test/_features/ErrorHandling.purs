module Test.Features.ErrorHandling where

import Prelude

import Effect (Effect)
import Effect.Aff (throwError)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)
import Yoga.Om (Om, mapError, onError, throw, runOm)

testOnError :: Effect ViTest
testOnError = describe "onError" do
  _ <- test "catches a specific error and recovers" do
    let
      failing :: Om {} (notFound :: String) String
      failing = throw { notFound: "missing" }
      recovered = failing
        # onError @"notFound" \msg -> pure ("recovered: " <> msg)
    result <- runOm {} { exception: throwError } recovered
    expectToBe "recovered: missing" result

  _ <- test "passes through when the error is not thrown" do
    let
      succeeding :: Om {} (notFound :: String) String
      succeeding = pure "ok"
      handled = succeeding
        # onError @"notFound" \_ -> pure "should not happen"
    result <- runOm {} { exception: throwError } handled
    expectToBe "ok" result

  _ <- test "leaves other errors intact" do
    let
      failing :: Om {} (notFound :: String, forbidden :: Unit) String
      failing = throw { forbidden: unit }
      handled = failing
        # onError @"notFound" \_ -> pure "recovered"
    result <- runOm {}
      { exception: throwError
      , forbidden: \_ -> pure "was forbidden"
      }
      handled
    expectToBe "was forbidden" result

  test "chains multiple onError handlers" do
    let
      failing :: Om {} (notFound :: String, badInput :: String) String
      failing = throw { badInput: "invalid" }
    result <- runOm {} { exception: throwError } do
      failing
        # onError @"notFound" (\msg -> pure ("not found: " <> msg))
        # onError @"badInput" (\msg -> pure ("bad input: " <> msg))
    expectToBe "bad input: invalid" result

testMapError :: Effect ViTest
testMapError = describe "mapError" do
  _ <- test "renames an error label" do
    let
      failing :: Om {} (notFound :: String) String
      failing = throw { notFound: "missing" }
      mapped = failing
        # mapError @"notFound" @"gone" ("was: " <> _)
    result <- runOm {}
      { exception: throwError
      , gone: \msg -> pure msg
      }
      mapped
    expectToBe "was: missing" result

  _ <- test "transforms the error value" do
    let
      failing :: Om {} (rawError :: Int) String
      failing = throw { rawError: 42 }
      mapped = failing
        # mapError @"rawError" @"displayError" show
    result <- runOm {}
      { exception: throwError
      , displayError: \msg -> pure msg
      }
      mapped
    expectToBe "42" result

  test "passes through on success" do
    let
      succeeding :: Om {} (notFound :: String) String
      succeeding = pure "ok"
      mapped = succeeding
        # mapError @"notFound" @"gone" ("was: " <> _)
    result <- runOm {}
      { exception: throwError
      , gone: \_ -> pure "should not happen"
      }
      mapped
    expectToBe "ok" result
