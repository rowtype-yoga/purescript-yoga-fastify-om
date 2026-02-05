module Test.Operators where

import Prelude
import Effect (Effect)
import ViTest (ViTest, viTest)
import OperatorTest.Spec as OperatorTest
import ParserTest.Spec as ParserTest
import RouteTest.Spec as RouteTest
import VariantResponseTest.Spec as VariantResponseTest
import HandleRouteTest.Spec as HandleRouteTest

spec :: Effect ViTest
spec = do
  _ <- OperatorTest.testRendering
  _ <- ParserTest.testSegmentParsing
  _ <- ParserTest.testCaptureParsing
  _ <- ParserTest.testPathParsing
  _ <- ParserTest.testOptionalQueryParams
  _ <- ParserTest.testRequiredQueryParams
  _ <- ParserTest.testErrorCases
  _ <- RouteTest.testHeaderValueString
  _ <- RouteTest.testHeaderValueInt
  _ <- RouteTest.testHeaderValueMaybe
  _ <- RouteTest.testRoundTrip
  _ <- RouteTest.testBearerToken
  _ <- RouteTest.testParseHeaders
  _ <- RouteTest.testRenderMethod
  _ <- RouteTest.testRenderHeadersSchema
  _ <- RouteTest.testRenderResponseHeadersSchema
  _ <- RouteTest.testRenderResponseSchema
  _ <- RouteTest.testToOpenAPI
  _ <- VariantResponseTest.testStatusCodeMapping
  _ <- VariantResponseTest.testStatusCodeToString
  _ <- VariantResponseTest.testRespondNoHeaders
  _ <- VariantResponseTest.testRespondWith
  _ <- VariantResponseTest.testRespond
  _ <- VariantResponseTest.testSimpleVariantOpenAPI
  _ <- VariantResponseTest.testComplexVariantOpenAPI
  _ <- VariantResponseTest.testVariantWithHeaders
  _ <- VariantResponseTest.testVariantPatternMatching
  _ <- HandleRouteTest.testParsePathParams
  _ <- HandleRouteTest.testParseQueryParams
  HandleRouteTest.testParseBody

main :: ViTest
main = viTest spec
