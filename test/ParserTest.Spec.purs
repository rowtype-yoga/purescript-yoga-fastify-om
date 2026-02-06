module ParserTest.Spec where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Test.ParserTest as Parser
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Path (Path, Required, type (/), type (:), type (:?))
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

-- Custom equality assertion that compares in PureScript then asserts true
expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

-- Type aliases for tests
type TestPathSimple = "users" / "id" : Int / "posts"
type TestPathWithQuery = Path ("users" / "id" : Int / "posts") :? { limit :: Int, offset :: Int }
type TestPathWithRequired = Path ("users" / "id" : Int / "posts") :? { limit :: Required Int, offset :: Int }
type TestPathError = Path ("users" / "id" : Int / "posts") :? { limit :: Int }

-- Test segment parsing
testSegmentParsing :: Effect ViTest
testSegmentParsing = describe "Segment Parsing" $ do
  _ <- test "matches literal segment" do
    let result = Parser.matchSegment (Proxy :: Proxy "users") "/users/124"
    expectToEqual (Just "/124") result

  test "fails to match different segment" do
    let result = Parser.matchSegment (Proxy :: Proxy "posts") "/users/124"
    expectToEqual Nothing result

-- Test capture parsing
testCaptureParsing :: Effect ViTest
testCaptureParsing = describe "Capture Parsing" $ do
  _ <- test "parses Int capture" do
    let result = Parser.parseCapture "/124/posts" :: Maybe { value :: Int, remaining :: String }
    expectToEqual (Just { value: 124, remaining: "/posts" }) result

  _ <- test "parses String capture" do
    let result = Parser.parseCapture "/hello/world" :: Maybe { value :: String, remaining :: String }
    expectToEqual (Just { value: "hello", remaining: "/world" }) result

  test "fails to parse invalid Int" do
    let result = Parser.parseCapture "/hello/world" :: Maybe { value :: Int, remaining :: String }
    expectToEqual Nothing result

-- Test complete path parsing
testPathParsing :: Effect ViTest
testPathParsing = describe "Path Parsing" $ do
  _ <- test "parses path with capture" do
    let result = Parser.parsePathSegments (Proxy :: Proxy TestPathSimple) "/users/124/posts"
    expectToEqual (Just { captures: { id: 124 }, remaining: "" }) result

  _ <- test "fails on invalid path" do
    let result = Parser.parsePathSegments (Proxy :: Proxy TestPathSimple) "/users/hello/posts"
    expectToEqual Nothing result

  test "fails on wrong path structure" do
    let result = Parser.parsePathSegments (Proxy :: Proxy TestPathSimple) "/wrong/124/posts"
    expectToEqual Nothing result

-- Test query parameter parsing (optional)
testOptionalQueryParams :: Effect ViTest
testOptionalQueryParams = describe "Optional Query Parameters" do
  _ <- test "parses all query params present" do
    let result = Parser.parseFullPath (Proxy :: Proxy TestPathWithQuery) (Proxy :: Proxy (limit :: Int, offset :: Int)) "/users/124/posts?limit=10&offset=20"
    expectToEqual (Right { path: { id: 124 }, query: { limit: Just 10, offset: Just 20 } }) result

  _ <- test "parses with some query params missing" do
    let result = Parser.parseFullPath (Proxy :: Proxy TestPathWithQuery) (Proxy :: Proxy (limit :: Int, offset :: Int)) "/users/124/posts?limit=10"
    expectToEqual (Right { path: { id: 124 }, query: { limit: Just 10, offset: Nothing } }) result

  test "parses with no query params" do
    let result = Parser.parseFullPath (Proxy :: Proxy TestPathWithQuery) (Proxy :: Proxy (limit :: Int, offset :: Int)) "/users/124/posts"
    expectToEqual (Right { path: { id: 124 }, query: { limit: Nothing, offset: Nothing } }) result

-- Test required query parameters
testRequiredQueryParams :: Effect ViTest
testRequiredQueryParams = describe "Required Query Parameters" $ do
  _ <- test "parses required param as plain type" do
    let result = Parser.parseFullPath (Proxy :: Proxy TestPathWithRequired) (Proxy :: Proxy (limit :: Required Int, offset :: Int)) "/users/124/posts?limit=10&offset=20"
    expectToEqual (Right { path: { id: 124 }, query: { limit: 10, offset: Just 20 } }) result

  _ <- test "succeeds when required param present, optional missing" do
    let result = Parser.parseFullPath (Proxy :: Proxy TestPathWithRequired) (Proxy :: Proxy (limit :: Required Int, offset :: Int)) "/users/124/posts?limit=10"
    expectToEqual (Right { path: { id: 124 }, query: { limit: 10, offset: Nothing } }) result

  _ <- test "fails when required param missing" do
    let result = Parser.parseFullPath (Proxy :: Proxy TestPathWithRequired) (Proxy :: Proxy (limit :: Required Int, offset :: Int)) "/users/124/posts"
    expectToEqual (Left [ "Missing required query parameter: limit" ]) result

  test "fails with descriptive error for missing required param" do
    let result = Parser.parseFullPath (Proxy :: Proxy TestPathWithRequired) (Proxy :: Proxy (limit :: Required Int, offset :: Int)) "/users/124/posts?offset=5"
    expectToEqual (Left [ "Missing required query parameter: limit" ]) result

-- Test error cases
testErrorCases :: Effect ViTest
testErrorCases = describe "Error Handling" $ do
  _ <- test "returns error for invalid path structure" do
    let result = Parser.parseFullPath (Proxy :: Proxy TestPathError) (Proxy :: Proxy (limit :: Int)) "/invalid/path"
    case result of
      Left _ -> expectToBe true true
      Right _ -> expectToBe false true

  test "handles invalid Int in capture" do
    let result = Parser.parsePathSegments (Proxy :: Proxy TestPathSimple) "/users/notanumber/posts"
    expectToEqual Nothing result
