module VariantResponseTest.Spec where

import Prelude

import Data.String as String
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))
import Yoga.Fastify.Fastify (StatusCode(..))
import Yoga.Fastify.Om.Path (Root)
import Yoga.Fastify.Om.Route (GET, POST, PUT, Route, Request, ResponseData(..), respondNoHeaders, respondWith, toOpenAPI, statusCodeFor, statusCodeToString)
import Yoga.Fastify.Om.Route.Response (respond) as Response
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

-- Custom equality assertion that compares in PureScript then asserts true
expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

--------------------------------------------------------------------------------
-- Type Definitions - Test that variant-based routes compile correctly
--------------------------------------------------------------------------------

-- Simple variant route with ok and notFound responses
type SimpleVariantRoute = Route GET Root
  (Request ())
  ( ok :: ResponseData () String
  , notFound :: ResponseData () String
  )

-- Variant route with multiple status codes
type MultiStatusRoute = Route POST Root
  (Request (headers :: { authorization :: String }))
  ( created :: ResponseData ("Location" :: String) User
  , badRequest :: ResponseData () ErrorMessage
  , unauthorized :: ResponseData () ErrorMessage
  )

-- Variant route with many response types
type ComplexVariantRoute = Route PUT Root
  (Request ())
  ( ok :: ResponseData () User
  , created :: ResponseData ("Location" :: String) User
  , badRequest :: ResponseData () ErrorMessage
  , notFound :: ResponseData () ErrorMessage
  , internalServerError :: ResponseData () ErrorMessage
  )

-- Example types for testing
type User = { id :: Int, name :: String }
type ErrorMessage = { error :: String }

--------------------------------------------------------------------------------
-- Status Code Mapping Tests
--------------------------------------------------------------------------------

testStatusCodeMapping :: Effect ViTest
testStatusCodeMapping = describe "StatusCodeMap" $ do
  _ <- test "ok maps to 200" do
    let statusCode = statusCodeFor (Proxy :: _ "ok")
    expectToEqual "200" (statusCodeToString statusCode)

  _ <- test "created maps to 201" do
    let statusCode = statusCodeFor (Proxy :: _ "created")
    expectToEqual "201" (statusCodeToString statusCode)

  _ <- test "accepted maps to 202" do
    let statusCode = statusCodeFor (Proxy :: _ "accepted")
    expectToEqual "202" (statusCodeToString statusCode)

  _ <- test "noContent maps to 204" do
    let statusCode = statusCodeFor (Proxy :: _ "noContent")
    expectToEqual "204" (statusCodeToString statusCode)

  _ <- test "badRequest maps to 400" do
    let statusCode = statusCodeFor (Proxy :: _ "badRequest")
    expectToEqual "400" (statusCodeToString statusCode)

  _ <- test "unauthorized maps to 401" do
    let statusCode = statusCodeFor (Proxy :: _ "unauthorized")
    expectToEqual "401" (statusCodeToString statusCode)

  _ <- test "forbidden maps to 403" do
    let statusCode = statusCodeFor (Proxy :: _ "forbidden")
    expectToEqual "403" (statusCodeToString statusCode)

  _ <- test "notFound maps to 404" do
    let statusCode = statusCodeFor (Proxy :: _ "notFound")
    expectToEqual "404" (statusCodeToString statusCode)

  _ <- test "conflict maps to 409" do
    let statusCode = statusCodeFor (Proxy :: _ "conflict")
    expectToEqual "409" (statusCodeToString statusCode)

  _ <- test "unprocessableEntity maps to 422" do
    let statusCode = statusCodeFor (Proxy :: _ "unprocessableEntity")
    expectToEqual "422" (statusCodeToString statusCode)

  _ <- test "internalServerError maps to 500" do
    let statusCode = statusCodeFor (Proxy :: _ "internalServerError")
    expectToEqual "500" (statusCodeToString statusCode)

  _ <- test "notImplemented maps to 501" do
    let statusCode = statusCodeFor (Proxy :: _ "notImplemented")
    expectToEqual "501" (statusCodeToString statusCode)

  _ <- test "badGateway maps to 502" do
    let statusCode = statusCodeFor (Proxy :: _ "badGateway")
    expectToEqual "502" (statusCodeToString statusCode)

  test "serviceUnavailable maps to 503" do
    let statusCode = statusCodeFor (Proxy :: _ "serviceUnavailable")
    expectToEqual "503" (statusCodeToString statusCode)

testStatusCodeToString :: Effect ViTest
testStatusCodeToString = describe "statusCodeToString" $ do
  _ <- test "converts 200 to string" do
    let result = statusCodeToString (StatusCode 200)
    expectToEqual "200" result

  _ <- test "converts 404 to string" do
    let result = statusCodeToString (StatusCode 404)
    expectToEqual "404" result

  test "converts 500 to string" do
    let result = statusCodeToString (StatusCode 500)
    expectToEqual "500" result

--------------------------------------------------------------------------------
-- Response Construction Tests
--------------------------------------------------------------------------------

testRespondNoHeaders :: Effect ViTest
testRespondNoHeaders = describe "respondNoHeaders" $ do
  _ <- test "constructs ok response with body" do
    let
      response :: Variant (ok :: ResponseData () String, notFound :: ResponseData () String)
      response = respondNoHeaders @"ok" "Success message"
      result = Variant.match
        { ok: \(ResponseData rd) -> rd.body
        , notFound: \_ -> "Wrong variant!"
        }
        response
    expectToEqual "Success message" result

  _ <- test "constructs notFound response with body" do
    let
      response :: Variant (ok :: ResponseData () String, notFound :: ResponseData () String)
      response = respondNoHeaders @"notFound" "Not found"
      result = Variant.match
        { ok: \_ -> "Wrong variant!"
        , notFound: \(ResponseData rd) -> rd.body
        }
        response
    expectToEqual "Not found" result

  test "response has empty headers" do
    let
      response :: Variant (ok :: ResponseData () String)
      response = respondNoHeaders @"ok" "Test"
      result = Variant.match
        { ok: \(ResponseData rd) -> rd.headers
        }
        response
    expectToEqual {} result

testRespondWith :: Effect ViTest
testRespondWith = describe "respondWith" $ do
  _ <- test "constructs response with headers and body" do
    let
      response :: Variant (created :: ResponseData ("Location" :: String) User)
      response = respondWith (Proxy :: _ "created")
        { "Location": "/users/123" }
        { id: 123, name: "Alice" }
      result = Variant.match
        { created: \(ResponseData rd) -> rd.body.id
        }
        response
    expectToEqual 123 result

  _ <- test "includes headers in response" do
    let
      response :: Variant (created :: ResponseData ("Location" :: String) User)
      response = respondWith (Proxy :: _ "created")
        { "Location": "/users/456" }
        { id: 456, name: "Bob" }
      result = Variant.match
        { created: \(ResponseData rd) -> rd.headers."Location"
        }
        response
    expectToEqual "/users/456" result

  test "constructs response with multiple headers" do
    let
      response :: Variant (ok :: ResponseData ("X-Request-Id" :: String, "X-Version" :: String) String)
      response = respondWith (Proxy :: _ "ok")
        { "X-Request-Id": "req-123"
        , "X-Version": "v1"
        }
        "Response body"
      reqId = Variant.match
        { ok: \(ResponseData rd) -> rd.headers."X-Request-Id"
        }
        response
      version = Variant.match
        { ok: \(ResponseData rd) -> rd.headers."X-Version"
        }
        response
    expectToBe true (reqId == "req-123" && version == "v1")

testRespond :: Effect ViTest
testRespond = describe "respond" $ do
  test "constructs response from ResponseData record" do
    let
      responseData :: ResponseData ("Location" :: String) User
      responseData = ResponseData
        { headers: { "Location": "/users/789" }
        , body: { id: 789, name: "Charlie" }
        }

      response :: Variant (created :: ResponseData ("Location" :: String) User)
      response = Response.respond (Proxy :: _ "created") responseData
      userId = Variant.match
        { created: \(ResponseData rd) -> rd.body.id
        }
        response
      location = Variant.match
        { created: \(ResponseData rd) -> rd.headers."Location"
        }
        response
    expectToBe true (userId == 789 && location == "/users/789")

--------------------------------------------------------------------------------
-- OpenAPI Generation Tests
--------------------------------------------------------------------------------

testSimpleVariantOpenAPI :: Effect ViTest
testSimpleVariantOpenAPI = describe "OpenAPI Generation - Simple Variant" $ do
  _ <- test "generates OpenAPI with multiple status codes" do
    let
      result = toOpenAPI
        @( Route GET Root
            (Request ())
            ( ok :: ResponseData () String
            , notFound :: ResponseData () String
            )
        )
    -- Verify contains both 200 and 404
    expectToBe true (String.contains (String.Pattern "200") result)
    expectToBe true (String.contains (String.Pattern "404") result)

  test "contains response structure" do
    let
      result = toOpenAPI @SimpleVariantRoute
    -- Verify OpenAPI structure
    expectToBe true (String.contains (String.Pattern "parameters") result)
    expectToBe true (String.contains (String.Pattern "responses") result)
    expectToBe true (String.contains (String.Pattern "Successful response") result)

testComplexVariantOpenAPI :: Effect ViTest
testComplexVariantOpenAPI = describe "OpenAPI Generation - Complex Variant" $ do
  _ <- test "generates OpenAPI for route with 3 status codes" do
    let
      result = toOpenAPI
        @( Route POST Root
            (Request (headers :: { authorization :: String }))
            ( created :: ResponseData ("Location" :: String) User
            , badRequest :: ResponseData () ErrorMessage
            , unauthorized :: ResponseData () ErrorMessage
            )
        )
    -- Verify contains 201, 400, and 401
    expectToBe true (String.contains (String.Pattern "201") result)
    expectToBe true (String.contains (String.Pattern "400") result)
    expectToBe true (String.contains (String.Pattern "401") result)

  _ <- test "includes request parameters" do
    let
      result = toOpenAPI @MultiStatusRoute
    -- Verify includes authorization header parameter
    expectToBe true (String.contains (String.Pattern "authorization") result)
    expectToBe true (String.contains (String.Pattern "header") result)

  test "generates OpenAPI for route with 5 status codes" do
    let
      result = toOpenAPI @ComplexVariantRoute
    -- Verify contains all status codes
    expectToBe true (String.contains (String.Pattern "200") result)
    expectToBe true (String.contains (String.Pattern "201") result)
    expectToBe true (String.contains (String.Pattern "400") result)
    expectToBe true (String.contains (String.Pattern "404") result)
    expectToBe true (String.contains (String.Pattern "500") result)

testVariantWithHeaders :: Effect ViTest
testVariantWithHeaders = describe "OpenAPI Generation - Variant with Response Headers" $ do
  test "includes response headers in OpenAPI" do
    let
      result = toOpenAPI
        @( Route POST Root
            (Request ())
            ( created :: ResponseData ("Location" :: String, "X-Request-Id" :: String) User
            )
        )
    -- Verify includes Location and X-Request-Id in response headers
    expectToBe true (String.contains (String.Pattern "Location") result)
    expectToBe true (String.contains (String.Pattern "X-Request-Id") result)
    expectToBe true (String.contains (String.Pattern "201") result)

--------------------------------------------------------------------------------
-- Integration Tests
--------------------------------------------------------------------------------

testVariantPatternMatching :: Effect ViTest
testVariantPatternMatching = describe "Variant Pattern Matching" $ do
  _ <- test "can pattern match on ok variant" do
    let
      response :: Variant (ok :: ResponseData () String, notFound :: ResponseData () String)
      response = respondNoHeaders @"ok" "Success"
      result = Variant.match
        { ok: \_ -> "matched ok"
        , notFound: \_ -> "matched notFound"
        }
        response
    expectToEqual "matched ok" result

  _ <- test "can pattern match on notFound variant" do
    let
      response :: Variant (ok :: ResponseData () String, notFound :: ResponseData () String)
      response = respondNoHeaders @"notFound" "Not found"
      result = Variant.match
        { ok: \_ -> "matched ok"
        , notFound: \_ -> "matched notFound"
        }
        response
    expectToEqual "matched notFound" result

  test "can match all cases with match" do
    let
      response :: Variant (ok :: ResponseData () String, notFound :: ResponseData () String, badRequest :: ResponseData () String)
      response = respondNoHeaders @"badRequest" "Bad request"
      result = Variant.match
        { ok: \_ -> "matched ok"
        , notFound: \_ -> "matched notFound"
        , badRequest: \_ -> "matched badRequest"
        }
        response
    expectToEqual "matched badRequest" result
