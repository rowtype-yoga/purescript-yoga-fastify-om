module Test.Features.RequestParsing where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Route.ParsePathParams (parsePathParams)
import Yoga.Fastify.Route.ParseQueryParams (parseQueryParamsFromObject)
import Yoga.Fastify.Route.ParseBody (parseBody)
import Yoga.HTTP.API.Route.Encoding as Encoding
import Unsafe.Coerce (unsafeCoerce)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

--------------------------------------------------------------------------------
-- ParsePathParams Tests
--------------------------------------------------------------------------------

testParsePathParams :: Effect ViTest
testParsePathParams = describe "ParsePathParams" $ do
  _ <- test "parses single Int param from Object" do
    let
      obj = Object.fromFoldable [ ("id" :: String) /\ "123" ]
      result = parsePathParams (Proxy :: Proxy (id :: Int)) obj
    expectToEqual (Right { id: 123 }) result

  _ <- test "parses multiple params from Object" do
    let
      obj = Object.fromFoldable [ ("userId" :: String) /\ "1", ("postId" :: String) /\ "42" ]
      result = parsePathParams (Proxy :: Proxy (userId :: Int, postId :: Int)) obj
    expectToEqual (Right { userId: 1, postId: 42 }) result

  _ <- test "returns error for missing param" do
    let
      obj = Object.empty :: Object.Object String
      result = parsePathParams (Proxy :: Proxy (id :: Int)) obj
    case result of
      Left errs -> expectToEqual [ "Missing path parameter: id" ] errs
      Right _ -> expectToBe true false

  _ <- test "returns error for invalid param" do
    let
      obj = Object.fromFoldable [ ("id" :: String) /\ "abc" ]
      result = parsePathParams (Proxy :: Proxy (id :: Int)) obj
    case result of
      Left _ -> expectToBe true true
      Right _ -> expectToBe true false

  _ <- test "accumulates multiple errors" do
    let
      obj = Object.empty :: Object.Object String
      result = parsePathParams (Proxy :: Proxy (userId :: Int, postId :: Int)) obj
    case result of
      Left errs -> expectToBe true (errs == [ "Missing path parameter: postId", "Missing path parameter: userId" ] || errs == [ "Missing path parameter: userId", "Missing path parameter: postId" ])
      Right _ -> expectToBe true false

  test "parses empty params" do
    let
      obj = Object.empty :: Object.Object String
      result = parsePathParams (Proxy :: Proxy ()) obj
    expectToEqual (Right {}) result

--------------------------------------------------------------------------------
-- ParseQueryParamsFromObject Tests
--------------------------------------------------------------------------------

testParseQueryParams :: Effect ViTest
testParseQueryParams = describe "ParseQueryParamsFromObject" $ do
  _ <- test "parses optional param as Just when present" do
    let
      obj = Object.fromFoldable [ ("limit" :: String) /\ (unsafeCoerce "10" :: _) ]
      result = parseQueryParamsFromObject (Proxy :: Proxy (limit :: Maybe Int)) obj
    expectToEqual (Right { limit: Just 10 }) result

  _ <- test "parses optional param as Nothing when absent" do
    let
      obj = Object.empty :: Object.Object _
      result = parseQueryParamsFromObject (Proxy :: Proxy (limit :: Maybe Int)) obj
    expectToEqual (Right { limit: Nothing :: Maybe Int }) result

  _ <- test "parses required param when present" do
    let
      obj = Object.fromFoldable [ ("offset" :: String) /\ (unsafeCoerce "20" :: _) ]
      result = parseQueryParamsFromObject (Proxy :: Proxy (offset :: Int)) obj
    expectToEqual (Right { offset: 20 }) result

  _ <- test "returns error for missing required param" do
    let
      obj = Object.empty :: Object.Object _
      result = parseQueryParamsFromObject (Proxy :: Proxy (offset :: Int)) obj
    case result of
      Left errs -> expectToEqual [ "Missing required query parameter: offset" ] errs
      Right _ -> expectToBe true false

  test "parses empty query params" do
    let
      obj = Object.empty :: Object.Object _
      result = parseQueryParamsFromObject (Proxy :: Proxy ()) obj
    expectToEqual (Right {}) result

--------------------------------------------------------------------------------
-- ParseBody Tests
--------------------------------------------------------------------------------

testParseBody :: Effect ViTest
testParseBody = describe "ParseBody" $ do
  _ <- test "NoBody always succeeds with unit" do
    let result = parseBody (Proxy :: Proxy Encoding.NoBody) Nothing
    expectToEqual (Right unit) result

  test "NoBody succeeds even with body present" do
    let result = parseBody (Proxy :: Proxy Encoding.NoBody) (Just (unsafeCoerce "ignored"))
    expectToEqual (Right unit) result
