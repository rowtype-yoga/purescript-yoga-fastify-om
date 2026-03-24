module Test.CompileFail where

import Prelude

import CompileFail (compileFile, spagoSources, warmCache)
import CompileFail.Golden (GoldenResult(..), checkGolden)
import CompileFail.HtmlDiff (writeDiffReport)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.Path as Path
import Test.Spec.Assertions (fail)
import ViTest (ViTest, beforeAll, describe, test)
import ViTest.Expect (expectToBe)

goldenDir :: String
goldenDir = "test-compile-fail/golden"

diffDir :: String
diffDir = "test-compile-fail/diffs"

compileFailCase :: Ref.Ref { sources :: Array String, outputDir :: String } -> String -> Effect ViTest
compileFailCase ctxRef filePath =
  test filePath do
    ctx <- liftEffect $ Ref.read ctxRef
    result <- compileFile ctx filePath
    expectToBe true result.compilationFailed
    let testName = Path.basenameWithoutExt filePath ".purs"
    goldenResult <- checkGolden { goldenDir, testName, actual: result.output }
    case goldenResult of
      GoldenMatch -> pure unit
      GoldenNew { goldenPath } ->
        fail $ "No golden file at " <> goldenPath <> ", run with UPDATE_GOLDEN=1 to create it"
      GoldenMismatch { expected, actual, goldenPath } -> do
        diffPath <- writeDiffReport { outputDir: diffDir, testName, expected, actual }
        fail $ "Golden mismatch for " <> goldenPath <> "\nDiff report: " <> diffPath

testCompileFail :: Effect ViTest
testCompileFail = do
  ctxRef <- liftEffect $ Ref.new { sources: [], outputDir: "output-compile-fail" }

  describe "Compile-fail tests" do
    beforeAll do
      sources <- spagoSources
      let ctx = { sources, outputDir: "output-compile-fail" }
      liftEffect $ Ref.write ctx ctxRef
      warmCache ctx

    _ <- compileFailCase ctxRef "test-compile-fail/cases/RegisterAPILayerExtraHandler.purs"
    _ <- compileFailCase ctxRef "test-compile-fail/cases/RegisterAPILayerMissingSqliteDependency.purs"
    _ <- compileFailCase ctxRef "test-compile-fail/cases/RegisterAPILayerMissingDependencyAcrossMultipleHandlers.purs"
    _ <- compileFailCase ctxRef "test-compile-fail/cases/RegisterAPILayerWrongDependencyType.purs"
    compileFailCase ctxRef "test-compile-fail/cases/RegisterAPILayerMixedMissingAndExtraHandlers.purs"
