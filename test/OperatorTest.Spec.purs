module OperatorTest.Spec where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Test.OperatorTest as Op
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Path (Path, type (/), type (:), type (:?))
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

-- Custom equality assertion that compares in PureScript then asserts true
expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

-- Test rendering paths to OpenAPI format
testRendering :: Effect ViTest
testRendering = describe "Path Rendering" $ do
  _ <- test "renders simple path with capture" do
    let result = Op.renderSimplePath (Proxy :: Proxy (Path ("users" / "id" : Int / "posts")))
    result # expectToEqual "/users/{id}/posts"

  _ <- test "renders path with query params" do
    let result = Op.renderFullPath (Proxy :: Proxy (Path ("api" / "posts") :? { page :: Int, sort :: String }))
    expectToEqual "/api/posts?page={page}&sort={sort}" result

  test "renders complex path with captures and query params" do
    let result = Op.renderFullPath (Proxy :: Proxy (Path ("api" / "users" / "id" : Int / "posts") :? { limit :: Int, offset :: Int }))
    expectToEqual "/api/users/{id}/posts?limit={limit}&offset={offset}" result
