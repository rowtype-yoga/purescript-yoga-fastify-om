module OperatorTest.Spec where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))
import ViTest (ViTest, describe, test, viTest)
import ViTest.Expect (expectToBe)

-- Import our operators and types
import Test.OperatorTest as Op

-- Helper operators for tests
infixr 8 type Op.Param as :>
infixr 6 type Op.PathCons as /
infixl 1 type Op.QueryParams as :?

-- Test rendering paths to OpenAPI format
testRendering :: Effect ViTest
testRendering = describe "Path Rendering" $ do
  _ <- test "renders simple path with capture" do
    let result = Op.renderSimplePath (Proxy :: Proxy (Op.Path ("users" / "id" :> Int / "posts")))
    expectToBe "/users/{id}/posts" result

  _ <- test "renders path with query params" do
    let result = Op.renderFullPath (Proxy :: Proxy (Op.Path ("api" / "posts") :? (page :: Int, sort :: String)))
    expectToBe "/api/posts?page={page}&sort={sort}" result

  test "renders complex path with captures and query params" do
    let result = Op.renderFullPath (Proxy :: Proxy (Op.Path ("api" / "users" / "id" :> Int / "posts") :? (limit :: Int, offset :: Int)))
    expectToBe "/api/users/{id}/posts?limit={limit}&offset={offset}" result
