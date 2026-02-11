module Test.Operators where

import Prelude
import Effect (Effect)
import ViTest (ViTest, viTest)
import Test.Features.Parsing as Parsing
import Test.Features.RequestParsing as RequestParsing
import Test.Features.Headers as Headers
import Test.Features.RequestBody as RequestBody
import Test.Features.Responses as Responses
import Test.Features.Validation as Validation
import Test.Features.ErrorHandling as ErrorHandling
import Test.Internal.Rendering as Rendering

spec :: Effect ViTest
spec = do
  -- 1. Path Parsing (Captures & Segments)
  _ <- Parsing.testSegmentParsing
  _ <- Parsing.testCaptureParsing
  _ <- Parsing.testPathParsing
  _ <- Parsing.testErrorCases
  _ <- RequestParsing.testParsePathParams

  -- 2. Query Parameters
  _ <- Parsing.testOptionalQueryParams
  _ <- Parsing.testRequiredQueryParams
  _ <- RequestParsing.testParseQueryParams

  -- 3. Headers (Including Auth)
  _ <- Headers.testHeaderValueString
  _ <- Headers.testHeaderValueInt
  _ <- Headers.testHeaderValueMaybe
  _ <- Headers.testRoundTrip
  _ <- Headers.testBearerToken
  _ <- Headers.testParseHeaders

  -- 4. Request Bodies
  _ <- RequestParsing.testParseBody
  _ <- RequestBody.testRequestBodyToStrom

  -- 5. Responses (Status Codes & Variants)
  _ <- Responses.testStatusCodeMapping
  _ <- Responses.testStatusCodeToString
  _ <- Responses.testRespondNoHeaders
  _ <- Responses.testRespondWith
  _ <- Responses.testRespond
  _ <- Responses.testVariantPatternMatching

  -- 6. Validation (Min/Max, Patterns)
  _ <- Validation.testPatternValidation
  _ <- Validation.testMinLengthValidation
  _ <- Validation.testMaxLengthValidation
  _ <- Validation.testMinimumValidation
  _ <- Validation.testMaximumValidation
  _ <- Validation.testComposedValidation

  -- 7. Error Handling (onError, mapError)
  _ <- ErrorHandling.testOnError
  _ <- ErrorHandling.testMapError

  -- Internal: OpenAPI Generation
  _ <- Rendering.testRendering
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
  Responses.testVariantWithHeaders

main :: ViTest
main = viTest spec
