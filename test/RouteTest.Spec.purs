module RouteTest.Spec where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as FObject
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Om.Path (Root)
import Yoga.Fastify.Om.Route (HeaderError(..), BearerToken(..), unBearerToken, class HeaderValue, parseHeader, printHeader, parseHeaders, Route, Request, Response, GET, POST, PUT, toOpenAPI, renderMethod, renderHeadersSchema, renderResponseHeadersSchema, renderResponseSchema)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)
import ViTest.Expect.Either (expectRight, expectIsLeft, expectLeftContains)

-- Custom equality assertion that compares in PureScript then asserts true
expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

--------------------------------------------------------------------------------
-- Variant-based Route Type Definitions
--------------------------------------------------------------------------------

-- Example types for variant routes
type User = { id :: Int, name :: String }
type ErrorMessage = { error :: String }

-- Route with variant responses (ok and notFound)
type TestRoute5 = Route GET Root
  (Request {})
  ( ok :: { body :: User }
  , notFound :: { body :: ErrorMessage }
  )

-- Route with variant responses including headers
type TestRoute6 = Route POST Root
  (Request { headers :: { authorization :: String } })
  ( created :: { headers :: { "Location" :: String }, body :: User }
  , badRequest :: { body :: ErrorMessage }
  , unauthorized :: { body :: ErrorMessage }
  )

--------------------------------------------------------------------------------
-- HeaderValue Tests
--------------------------------------------------------------------------------

testHeaderValueString :: Effect ViTest
testHeaderValueString = describe "HeaderValue String" $ do
  _ <- test "parseHeader returns Right for valid string" do
    let result = (parseHeader "hello" :: Either String String)
    expectRight "hello" result

  test "printHeader returns identity" do
    let result = (printHeader "world" :: String)
    expectToEqual "world" result

testHeaderValueInt :: Effect ViTest
testHeaderValueInt = describe "HeaderValue Int" $ do
  _ <- test "parseHeader parses valid integer" do
    let result = (parseHeader "42" :: Either String Int)
    expectRight 42 result

  _ <- test "parseHeader returns Left with error for invalid integer" do
    let result = (parseHeader "not-a-number" :: Either String Int)
    expectLeftContains "not a valid integer" result

  test "printHeader converts int to string" do
    let result = (printHeader 123 :: String)
    expectToEqual "123" result

testHeaderValueMaybe :: Effect ViTest
testHeaderValueMaybe = describe "HeaderValue Maybe" $ do
  _ <- test "parseHeader wraps result in Just for valid input" do
    let result = (parseHeader "test" :: Either String (Maybe String))
    expectRight (Just "test") result

  _ <- test "printHeader handles Nothing" do
    let result = printHeader (Nothing :: Maybe String)
    expectToEqual "" result

  test "printHeader handles Just value" do
    let result = printHeader (Just "value" :: Maybe String)
    expectToEqual "value" result

--------------------------------------------------------------------------------
-- Round-trip Tests
--------------------------------------------------------------------------------

testRoundTrip :: Effect ViTest
testRoundTrip = describe "HeaderValue Round-trip" $ do
  _ <- test "String round-trips correctly" do
    let original = "test-value"
    let result = parseHeader (printHeader original) :: Either String String
    expectRight original result

  test "Int round-trips correctly" do
    let original = 42
    let result = parseHeader (printHeader original) :: Either String Int
    expectRight original result

--------------------------------------------------------------------------------
-- BearerToken Tests
--------------------------------------------------------------------------------

testBearerToken :: Effect ViTest
testBearerToken = describe "BearerToken" $ do
  _ <- test "parses valid Bearer token" do
    let result = parseHeader "Bearer abc123xyz" :: Either String BearerToken
    case result of
      Right token -> expectToEqual "abc123xyz" (unBearerToken token)
      Left _ -> expectToBe false true

  _ <- test "rejects token without Bearer prefix" do
    let result = parseHeader "abc123xyz" :: Either String BearerToken
    expectLeftContains "missing 'Bearer ' prefix" result

  _ <- test "rejects empty string" do
    let result = parseHeader "" :: Either String BearerToken
    expectIsLeft result

  _ <- test "handles Bearer with empty token" do
    let result = parseHeader "Bearer " :: Either String BearerToken
    case result of
      Right token -> expectToEqual "" (unBearerToken token)
      Left _ -> expectToBe false true

  test "round-trips correctly" do
    let original = BearerToken "secret-token-123"
    let result = parseHeader (printHeader original) :: Either String BearerToken
    expectRight original result

--------------------------------------------------------------------------------
-- ParseHeaders Tests (Error Accumulation)
--------------------------------------------------------------------------------

testParseHeaders :: Effect ViTest
testParseHeaders = describe "ParseHeaders (Error Accumulation)" $ do
  _ <- test "parses empty headers" do
    let obj = FObject.empty
    let result = parseHeaders (Proxy :: Proxy ()) obj
    expectToEqual (Right {}) result

  _ <- test "parses single string header" do
    let obj = FObject.fromFoldable [ "authorization" /\ "Bearer token123" ]
    let result = parseHeaders (Proxy :: Proxy (authorization :: String)) obj
    expectRight { authorization: "Bearer token123" } result

  _ <- test "parses multiple headers" do
    let
      obj = FObject.fromFoldable
        [ "authorization" /\ "Bearer token"
        , "x-api-version" /\ "42"
        ]
    let result = parseHeaders (Proxy :: Proxy (authorization :: String, "x-api-version" :: Int)) obj
    expectRight { authorization: "Bearer token", "x-api-version": 42 } result

  _ <- test "returns error for missing required header" do
    let obj = FObject.empty
    let result = parseHeaders (Proxy :: Proxy (authorization :: String)) obj
    expectIsLeft result

  _ <- test "returns error for invalid header value" do
    let obj = FObject.fromFoldable [ "x-version" /\ "not-a-number" ]
    let result = parseHeaders (Proxy :: Proxy ("x-version" :: Int)) obj
    expectIsLeft result

  test "accumulates multiple errors" do
    let obj = FObject.fromFoldable [ "x-version" /\ "bad-int" ]
    let result = parseHeaders (Proxy :: Proxy (authorization :: String, "x-version" :: Int, "x-api-key" :: String)) obj
    expectIsLeft result

--------------------------------------------------------------------------------
-- OpenAPI Generation Tests
--------------------------------------------------------------------------------

testRenderMethod :: Effect ViTest
testRenderMethod = describe "RenderMethod" $ do
  _ <- test "renders GET as lowercase" do
    let result = renderMethod (Proxy :: Proxy GET)
    expectToEqual "get" result

  _ <- test "renders POST as lowercase" do
    let result = renderMethod (Proxy :: Proxy POST)
    expectToEqual "post" result

  test "renders PUT as lowercase" do
    let result = renderMethod (Proxy :: Proxy PUT)
    expectToEqual "put" result

testRenderHeadersSchema :: Effect ViTest
testRenderHeadersSchema = describe "RenderHeadersSchema" $ do
  _ <- test "renders empty headers" do
    let result = renderHeadersSchema (Proxy :: Proxy ())
    expectToEqual [] result

  _ <- test "renders single String header" do
    let result = renderHeadersSchema (Proxy :: Proxy (authorization :: String))
    expectToBe true (result == [ { name: "authorization", in: "header", required: true, schema: { type: "string" } } ])

  test "renders Int header" do
    let result = renderHeadersSchema (Proxy :: Proxy ("x-version" :: Int))
    expectToBe true (result == [ { name: "x-version", in: "header", required: true, schema: { type: "integer" } } ])

testRenderResponseHeadersSchema :: Effect ViTest
testRenderResponseHeadersSchema = describe "RenderResponseHeadersSchema" $ do
  _ <- test "renders empty response headers" do
    let result = renderResponseHeadersSchema (Proxy :: Proxy ())
    expectToEqual FObject.empty result

  _ <- test "renders single String response header" do
    let result = renderResponseHeadersSchema (Proxy :: Proxy ("Location" :: String))
    let location = FObject.lookup "Location" result
    case location of
      Just header -> expectToEqual "string" header.schema.type
      Nothing -> expectToBe false true

  test "renders Int response header" do
    let result = renderResponseHeadersSchema (Proxy :: Proxy ("X-Version" :: Int))
    let version = FObject.lookup "X-Version" result
    case version of
      Just header -> expectToEqual "integer" header.schema.type
      Nothing -> expectToBe false true

testRenderResponseSchema :: Effect ViTest
testRenderResponseSchema = describe "RenderResponseSchema" $ do
  _ <- test "renders response with no headers" do
    let result = renderResponseSchema (Proxy :: Proxy ()) (Proxy :: Proxy String)
    expectToEqual "Successful response" result."200".description
    expectToEqual FObject.empty result."200".headers
    expectToEqual "object" result."200".content."application/json".schema.type

  test "renders response with headers" do
    let result = renderResponseSchema (Proxy :: Proxy ("Location" :: String, "X-Request-Id" :: String)) (Proxy :: Proxy Unit)
    expectToEqual "Successful response" result."200".description
    expectToBe true (FObject.member "Location" result."200".headers)
    expectToBe true (FObject.member "X-Request-Id" result."200".headers)
    expectToEqual "object" result."200".content."application/json".schema.type

testToOpenAPI :: Effect ViTest
testToOpenAPI = describe "ToOpenAPI" $ do
  _ <- test "generates OpenAPI for variant route with multiple status codes" do
    let result = toOpenAPI @TestRoute5
    -- Check that it contains both 200 and 404 status codes
    expectToBe true (String.contains (String.Pattern "200") result)
    expectToBe true (String.contains (String.Pattern "404") result)

  test "generates OpenAPI for variant route with request headers and multiple responses" do
    let result = toOpenAPI @TestRoute6
    -- Check that it contains authorization parameter
    expectToBe true (String.contains (String.Pattern "authorization") result)
    -- Check that it contains 201, 400, and 401 status codes
    expectToBe true (String.contains (String.Pattern "201") result)
    expectToBe true (String.contains (String.Pattern "400") result)
    expectToBe true (String.contains (String.Pattern "401") result)
