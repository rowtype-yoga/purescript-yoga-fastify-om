module MetadataValidationTest.Spec where

import Prelude

import Data.Either (Either(..), isLeft)
import Effect (Effect)
import Effect.Aff (Aff)
import Type.Function (type (#))
import Yoga.HTTP.API.Path (parseParam)
import Yoga.HTTP.API.Route.HeaderValue (parseHeader)
import Yoga.HTTP.API.Route.OpenAPIMetadata (Pattern, MinLength, MaxLength, Minimum, Maximum, Description, Example)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

expectLeft :: forall a b. Either a b -> Aff Unit
expectLeft = expectToBe true <<< isLeft

--------------------------------------------------------------------------------
-- Pattern Validation Tests
--------------------------------------------------------------------------------

testPatternValidation :: Effect ViTest
testPatternValidation = describe "Pattern Validation" $ do
  _ <- test "parseParam accepts matching string" do
    let result = parseParam "hello" :: Either String (String # Pattern "^[a-z]+$")
    expectToEqual (Right "hello") (map (const "hello") result)

  _ <- test "parseParam rejects non-matching string" do
    let result = parseParam "Hello123" :: Either String (String # Pattern "^[a-z]+$")
    expectLeft result

  _ <- test "parseParam error contains pattern" do
    let result = parseParam "123" :: Either String (String # Pattern "^[a-z]+$")
    case result of
      Left err -> expectToBe true (err == "Value '123' does not match pattern: ^[a-z]+$")
      Right _ -> expectToBe true false

  _ <- test "parseHeader accepts matching string" do
    let result = parseHeader "hello" :: Either String (String # Pattern "^[a-z]+$")
    expectToEqual (Right "hello") (map (const "hello") result)

  test "parseHeader rejects non-matching string" do
    let result = parseHeader "Hello123" :: Either String (String # Pattern "^[a-z]+$")
    expectLeft result

--------------------------------------------------------------------------------
-- MinLength Validation Tests
--------------------------------------------------------------------------------

testMinLengthValidation :: Effect ViTest
testMinLengthValidation = describe "MinLength Validation" $ do
  _ <- test "parseParam accepts string at minimum length" do
    let result = parseParam "abc" :: Either String (String # MinLength 3)
    expectToEqual (Right "abc") (map (const "abc") result)

  _ <- test "parseParam accepts string above minimum length" do
    let result = parseParam "abcdef" :: Either String (String # MinLength 3)
    expectToEqual (Right "abcdef") (map (const "abcdef") result)

  _ <- test "parseParam rejects string below minimum length" do
    let result = parseParam "ab" :: Either String (String # MinLength 3)
    expectLeft result

  _ <- test "parseParam error message for short string" do
    let result = parseParam "ab" :: Either String (String # MinLength 3)
    case result of
      Left err -> expectToBe true (err == "String length 2 is less than minimum 3")
      Right _ -> expectToBe true false

  test "parseHeader rejects string below minimum length" do
    let result = parseHeader "ab" :: Either String (String # MinLength 3)
    expectLeft result

--------------------------------------------------------------------------------
-- MaxLength Validation Tests
--------------------------------------------------------------------------------

testMaxLengthValidation :: Effect ViTest
testMaxLengthValidation = describe "MaxLength Validation" $ do
  _ <- test "parseParam accepts string at maximum length" do
    let result = parseParam "abc" :: Either String (String # MaxLength 3)
    expectToEqual (Right "abc") (map (const "abc") result)

  _ <- test "parseParam accepts string below maximum length" do
    let result = parseParam "ab" :: Either String (String # MaxLength 3)
    expectToEqual (Right "ab") (map (const "ab") result)

  _ <- test "parseParam rejects string above maximum length" do
    let result = parseParam "abcdef" :: Either String (String # MaxLength 3)
    expectLeft result

  _ <- test "parseParam error message for long string" do
    let result = parseParam "abcdef" :: Either String (String # MaxLength 3)
    case result of
      Left err -> expectToBe true (err == "String length 6 exceeds maximum 3")
      Right _ -> expectToBe true false

  test "parseHeader rejects string above maximum length" do
    let result = parseHeader "abcdef" :: Either String (String # MaxLength 3)
    expectLeft result

--------------------------------------------------------------------------------
-- Minimum Validation Tests
--------------------------------------------------------------------------------

testMinimumValidation :: Effect ViTest
testMinimumValidation = describe "Minimum Validation" $ do
  _ <- test "parseParam accepts value at minimum" do
    let result = parseParam "1" :: Either String (Int # Minimum 1)
    expectToEqual (Right 1) (map (const 1) result)

  _ <- test "parseParam accepts value above minimum" do
    let result = parseParam "50" :: Either String (Int # Minimum 1)
    expectToEqual (Right 50) (map (const 50) result)

  _ <- test "parseParam rejects value below minimum" do
    let result = parseParam "0" :: Either String (Int # Minimum 1)
    expectLeft result

  _ <- test "parseParam error message for value below minimum" do
    let result = parseParam "0" :: Either String (Int # Minimum 1)
    case result of
      Left err -> expectToBe true (err == "Value 0 is less than minimum 1")
      Right _ -> expectToBe true false

  _ <- test "inner parse failure propagates" do
    let result = parseParam "abc" :: Either String (Int # Minimum 1)
    expectLeft result

  test "parseHeader rejects value below minimum" do
    let result = parseHeader "0" :: Either String (Int # Minimum 1)
    expectLeft result

--------------------------------------------------------------------------------
-- Maximum Validation Tests
--------------------------------------------------------------------------------

testMaximumValidation :: Effect ViTest
testMaximumValidation = describe "Maximum Validation" $ do
  _ <- test "parseParam accepts value at maximum" do
    let result = parseParam "100" :: Either String (Int # Maximum 100)
    expectToEqual (Right 100) (map (const 100) result)

  _ <- test "parseParam accepts value below maximum" do
    let result = parseParam "50" :: Either String (Int # Maximum 100)
    expectToEqual (Right 50) (map (const 50) result)

  _ <- test "parseParam rejects value above maximum" do
    let result = parseParam "150" :: Either String (Int # Maximum 100)
    expectLeft result

  _ <- test "parseParam error message for value above maximum" do
    let result = parseParam "150" :: Either String (Int # Maximum 100)
    case result of
      Left err -> expectToBe true (err == "Value 150 exceeds maximum 100")
      Right _ -> expectToBe true false

  test "parseHeader rejects value above maximum" do
    let result = parseHeader "150" :: Either String (Int # Maximum 100)
    expectLeft result

--------------------------------------------------------------------------------
-- Composed Validation Tests
--------------------------------------------------------------------------------

testComposedValidation :: Effect ViTest
testComposedValidation = describe "Composed Validation" $ do
  _ <- test "Int with Minimum and Maximum accepts valid value" do
    let result = parseParam "50" :: Either String (Int # Minimum 1 # Maximum 100)
    expectToEqual (Right 50) (map (const 50) result)

  _ <- test "Int with Minimum and Maximum rejects below minimum" do
    let result = parseParam "0" :: Either String (Int # Minimum 1 # Maximum 100)
    expectLeft result

  _ <- test "Int with Minimum and Maximum rejects above maximum" do
    let result = parseParam "150" :: Either String (Int # Minimum 1 # Maximum 100)
    expectLeft result

  _ <- test "String with MinLength and MaxLength accepts valid string" do
    let result = parseParam "hello" :: Either String (String # MinLength 3 # MaxLength 10)
    expectToEqual (Right "hello") (map (const "hello") result)

  _ <- test "String with MinLength and MaxLength rejects short string" do
    let result = parseParam "ab" :: Either String (String # MinLength 3 # MaxLength 10)
    expectLeft result

  _ <- test "String with MinLength and MaxLength rejects long string" do
    let result = parseParam "this is too long" :: Either String (String # MinLength 3 # MaxLength 10)
    expectLeft result

  _ <- test "String with Pattern and MinLength validates both" do
    let result = parseParam "ab" :: Either String (String # Pattern "^[a-z]+$" # MinLength 3)
    expectLeft result

  _ <- test "documentary wrappers remain transparent" do
    let result = parseParam "42" :: Either String (Int # Description "user id" # Example "42" # Minimum 1)
    expectToEqual (Right 42) (map (const 42) result)

  test "inner parse failure propagates through composed wrappers" do
    let result = parseParam "abc" :: Either String (Int # Minimum 1 # Maximum 100)
    expectLeft result
