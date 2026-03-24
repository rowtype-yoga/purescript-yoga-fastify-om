module Test.CompileFail where

import Prelude

import CompileFail (compileFile, spagoSources, warmCache)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import ViTest (ViTest, beforeAll, describe, test)
import ViTest.Expect (expectToBe)

compileFailCase :: Ref.Ref { sources :: Array String, outputDir :: String } -> String -> Effect ViTest
compileFailCase ctxRef filePath =
  test filePath do
    ctx <- liftEffect $ Ref.read ctxRef
    result <- compileFile ctx filePath
    expectToBe true result.compilationFailed
    expectToBe true result.containsExpected

compilePassCase :: Ref.Ref { sources :: Array String, outputDir :: String } -> String -> Effect ViTest
compilePassCase ctxRef filePath =
  test filePath do
    ctx <- liftEffect $ Ref.read ctxRef
    result <- compileFile ctx filePath
    expectToBe false result.compilationFailed

testCompileFail :: Effect ViTest
testCompileFail = do
  ctxRef <- liftEffect $ Ref.new { sources: [], outputDir: "output-compile-fail" }

  describe "Compile-fail tests" do
    beforeAll do
      sources <- spagoSources
      let ctx = { sources, outputDir: "output-compile-fail" }
      liftEffect $ Ref.write ctx ctxRef
      warmCache ctx

    _ <- compilePassCase ctxRef "test-compile-fail/cases/RegisterAPILayerComplexHappyPath.purs"
    _ <- compileFailCase ctxRef "test-compile-fail/cases/RegisterAPILayerExtraHandler.purs"
    _ <- compileFailCase ctxRef "test-compile-fail/cases/RegisterAPILayerMissingSqliteDependency.purs"
    _ <- compileFailCase ctxRef "test-compile-fail/cases/RegisterAPILayerMissingDependencyAcrossMultipleHandlers.purs"
    _ <- compileFailCase ctxRef "test-compile-fail/cases/RegisterAPILayerWrongDependencyType.purs"
    compileFailCase ctxRef "test-compile-fail/cases/RegisterAPILayerMixedMissingAndExtraHandlers.purs"
