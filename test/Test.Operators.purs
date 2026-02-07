module Test.Operators where

import Prelude
import Effect (Effect)
import ViTest (ViTest, viTest)
import Test.Rendering as Rendering
import Test.Parsing as Parsing
import Test.HandleRoute as HandleRoute
import Test.Headers as Headers
import Test.RequestBody as RequestBody
import Test.Responses as Responses
import Test.Validation as Validation
import Test.Integration as Integration

spec :: Effect ViTest
spec = do
  -- 1. Basic Building Blocks
  _ <- Rendering.testRendering
  _ <- Parsing.testSegmentParsing
  _ <- Parsing.testCaptureParsing

  -- 2. Path Parsing
  _ <- Parsing.testPathParsing
  _ <- Parsing.testErrorCases
  _ <- HandleRoute.testParsePathParams

  -- 3. Query Parameters
  _ <- Parsing.testOptionalQueryParams
  _ <- Parsing.testRequiredQueryParams
  _ <- HandleRoute.testParseQueryParams

  -- 4. Headers
  _ <- Headers.testHeaderValueString
  _ <- Headers.testHeaderValueInt
  _ <- Headers.testHeaderValueMaybe
  _ <- Headers.testRoundTrip
  _ <- Headers.testBearerToken
  _ <- Headers.testParseHeaders

  -- 5. Request Bodies
  _ <- HandleRoute.testParseBody
  _ <- RequestBody.testRequestBodyToStrom

  -- 6. Responses
  _ <- Responses.testStatusCodeMapping
  _ <- Responses.testStatusCodeToString
  _ <- Responses.testRespondNoHeaders
  _ <- Responses.testRespondWith
  _ <- Responses.testRespond
  _ <- Responses.testVariantPatternMatching

  -- 7. Validation
  _ <- Validation.testPatternValidation
  _ <- Validation.testMinLengthValidation
  _ <- Validation.testMaxLengthValidation
  _ <- Validation.testMinimumValidation
  _ <- Validation.testMaximumValidation
  _ <- Validation.testComposedValidation

  -- 8. OpenAPI Generation
  _ <- Headers.testRenderMethod
  _ <- Headers.testRenderHeadersSchema
  _ <- Headers.testRenderResponseHeadersSchema
  _ <- Headers.testRenderResponseSchema
  _ <- Headers.testRenderPathParamsSchema
  _ <- Headers.testRenderQueryParamsSchema
  _ <- Headers.testRenderRequestBodySchema
  _ <- Headers.testToOpenAPI
  _ <- Responses.testSimpleVariantOpenAPI
  _ <- Responses.testComplexVariantOpenAPI
  _ <- Responses.testVariantWithHeaders

  -- 9. Full Server Integration
  Integration.testServerCompilation

main :: ViTest
main = viTest spec
