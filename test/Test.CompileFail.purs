module Test.CompileFail where

import Prelude

import CompileFail (checkCompileFileWithGolden, spagoSources, warmCache)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.Path as Path
import Test.Spec.Assertions (fail)
import ViTest (ViTest, beforeAll, describe, test)

goldenDir :: String
goldenDir = "test-compile-fail/golden"

diffDir :: String
diffDir = "test-compile-fail/diffs"

compileFailCase :: Ref.Ref { sources :: Array String, outputDir :: String } -> String -> Effect ViTest
compileFailCase ctxRef filePath =
  test filePath do
    ctx <- liftEffect $ Ref.read ctxRef
    result <- checkCompileFileWithGolden ctx
      { goldenDir
      , diffDir
      , testName: Path.basenameWithoutExt filePath ".purs"
      , filePath
      }
    case result of
      Right _ -> pure unit
      Left message -> fail message

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
