module Test.CompileFail where

import Prelude

import CompileFail (compileFile, spagoSources, warmCache)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import ViTest (ViTest, beforeAll, describe, test)
import ViTest.Expect (expectToBe)
import ViTest.Expect.String (expectContains)

type CompileFailCaseSpec =
  { filePath :: String
  , expected :: String
  }

compileFailCase :: Ref.Ref { sources :: Array String, outputDir :: String } -> CompileFailCaseSpec -> Effect ViTest
compileFailCase ctxRef { filePath, expected } =
  test filePath do
    ctx <- liftEffect $ Ref.read ctxRef
    result <- compileFile ctx filePath
    expectToBe true result.compilationFailed
    expectContains expected result.output

testCompileFail :: Effect ViTest
testCompileFail = do
  ctxRef <- liftEffect $ Ref.new { sources: [], outputDir: "output-compile-fail" }

  describe "Compile-fail tests" do
    beforeAll do
      sources <- spagoSources
      let ctx = { sources, outputDir: "output-compile-fail" }
      liftEffect $ Ref.write ctx ctxRef
      warmCache ctx

    _ <- compileFailCase ctxRef
      { filePath: "test-compile-fail/cases/RegisterAPILayerExtraHandler.purs"
      , expected: """while checking that expression (registerAPILayer) { health: healthHandler
                                                  , extra: extraHandler
                                                  }
  has type OmLayer"""
      }
    _ <- compileFailCase ctxRef
      { filePath: "test-compile-fail/cases/RegisterAPILayerMissingSqliteDependency.purs"
      , expected: """while solving type class constraint

  Prim.Row.Cons "sqlite"
                Connection"""
      }
    _ <- compileFailCase ctxRef
      { filePath: "test-compile-fail/cases/RegisterAPILayerMissingDependencyAcrossMultipleHandlers.purs"
      , expected: """while checking that expression (registerAPILayer) { health: healthHandler
                                                  , metrics: metricsHandler
                                                  }
  has type OmLayer"""
      }
    _ <- compileFailCase ctxRef
      { filePath: "test-compile-fail/cases/RegisterAPILayerWrongDependencyType.purs"
      , expected: """while matching label sqlite
while trying to match type
                             ( sqlite :: Connection"""
      }
    compileFailCase ctxRef
      { filePath: "test-compile-fail/cases/RegisterAPILayerMixedMissingAndExtraHandlers.purs"
      , expected: """while checking that expression (registerAPILayer) { health: healthHandler
                                                  , ghost: ghostHandler
                                                  }
  has type OmLayer"""
      }
