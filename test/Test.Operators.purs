module Test.Operators where

import Prelude
import Effect (Effect)
import ViTest (ViTest, viTest)
import OperatorTest.Spec as OperatorTest
import ParserTest.Spec as ParserTest
import RequestBodyTest.Spec as RequestBodyTest
import RouteTest.Spec as RouteTest
import Test.ServerIntegration.Spec as ServerIntegration
import VariantResponseTest.Spec as VariantResponseTest
import HandleRouteTest.Spec as HandleRouteTest
import MetadataValidationTest.Spec as MetadataValidationTest

spec :: Effect ViTest
spec = do
  -- 1. Basic Building Blocks
  _ <- OperatorTest.testRendering
  _ <- ParserTest.testSegmentParsing
  _ <- ParserTest.testCaptureParsing

  -- 2. Path Parsing
  _ <- ParserTest.testPathParsing
  _ <- ParserTest.testErrorCases
  _ <- HandleRouteTest.testParsePathParams

  -- 3. Query Parameters
  _ <- ParserTest.testOptionalQueryParams
  _ <- ParserTest.testRequiredQueryParams
  _ <- HandleRouteTest.testParseQueryParams

  -- 4. Headers
  _ <- RouteTest.testHeaderValueString
  _ <- RouteTest.testHeaderValueInt
  _ <- RouteTest.testHeaderValueMaybe
  _ <- RouteTest.testRoundTrip
  _ <- RouteTest.testBearerToken
  _ <- RouteTest.testParseHeaders

  -- 5. Request Bodies
  _ <- HandleRouteTest.testParseBody
  _ <- RequestBodyTest.testRequestBodyToStrom

  -- 6. Responses
  _ <- VariantResponseTest.testStatusCodeMapping
  _ <- VariantResponseTest.testStatusCodeToString
  _ <- VariantResponseTest.testRespondNoHeaders
  _ <- VariantResponseTest.testRespondWith
  _ <- VariantResponseTest.testRespond
  _ <- VariantResponseTest.testVariantPatternMatching

  -- 7. Validation
  _ <- MetadataValidationTest.testPatternValidation
  _ <- MetadataValidationTest.testMinLengthValidation
  _ <- MetadataValidationTest.testMaxLengthValidation
  _ <- MetadataValidationTest.testMinimumValidation
  _ <- MetadataValidationTest.testMaximumValidation
  _ <- MetadataValidationTest.testComposedValidation

  -- 8. OpenAPI Generation
  _ <- RouteTest.testRenderMethod
  _ <- RouteTest.testRenderHeadersSchema
  _ <- RouteTest.testRenderResponseHeadersSchema
  _ <- RouteTest.testRenderResponseSchema
  _ <- RouteTest.testRenderPathParamsSchema
  _ <- RouteTest.testRenderQueryParamsSchema
  _ <- RouteTest.testRenderRequestBodySchema
  _ <- RouteTest.testToOpenAPI
  _ <- VariantResponseTest.testSimpleVariantOpenAPI
  _ <- VariantResponseTest.testComplexVariantOpenAPI
  _ <- VariantResponseTest.testVariantWithHeaders

  -- 9. Full Server Integration
  ServerIntegration.testServerCompilation

main :: ViTest
main = viTest spec
