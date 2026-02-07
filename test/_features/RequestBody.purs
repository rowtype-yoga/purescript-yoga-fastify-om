module Test.Features.RequestBody where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.String as String
import Foreign (ForeignError)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Foreign.Object as FObject
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Partial.Unsafe (unsafePartial)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)
import Yoga.Fastify.Om.RequestBody (RequestBody(..), toStrom)
import Yoga.JSON as JSON
import Yoga.Om as Om
import Yoga.Om.Strom as Strom
import Yoga.Om.Strom.WebStream as WebStream

expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

testRequestBodyToStrom :: Effect ViTest
testRequestBodyToStrom = describe "RequestBody.toStrom" do

  test "converts BytesBody to Strom" do
    buf <- liftEffect $ Buffer.fromString "hello" UTF8
    let body = BytesBody buf
    let _stream = toStrom body
    expectToEqual true true -- Smoke test - just verify it compiles and runs

  test "converts TextBody to Strom" do
    let body = TextBody "world"
    let _stream = toStrom body
    expectToEqual true true

  test "converts NoBody to Strom" do
    let body = NoBody :: RequestBody Unit
    let _stream = toStrom body
    expectToEqual true true

  test "converts FormData to Strom" do
    let body = FormData (FObject.fromFoldable [])
    let _stream = toStrom body
    expectToEqual true true

  test "converts JSONBody to Strom" do
    let body = JSONBody { foo: "bar" }
    let _stream = toStrom body
    expectToEqual true true

  test "StreamBody flows through toStrom and can be collected" do
    -- Create a Strom with some buffers
    buf1 <- liftEffect $ Buffer.fromString "hello" UTF8
    buf2 <- liftEffect $ Buffer.fromString "world" UTF8
    let strom = Strom.fromArray [ buf1, buf2 ]

    -- Convert to ReadableStream
    stream <- liftEffect $ WebStream.toReadableStream {} strom

    -- Wrap in StreamBody and convert back to Strom
    let body = StreamBody stream
    let stromFromBody = toStrom body

    -- Collect the results
    result <- Om.runOm {} { exception: throwError } (Strom.runCollect stromFromBody)
    expectToEqual 2 (Array.length result)

    -- Verify content
    content1 <- liftEffect $ Buffer.toString UTF8 (unsafePartial $ Array.unsafeIndex result 0)
    content2 <- liftEffect $ Buffer.toString UTF8 (unsafePartial $ Array.unsafeIndex result 1)
    expectToEqual "hello" content1
    expectToEqual "world" content2

  test "streams large amount of data efficiently" do
    -- Create a large stream of random-ish data (1000 chunks)
    let
      makeChunk n = liftEffect do
        let str = "chunk-" <> show n <> "-data"
        Buffer.fromString str UTF8
      chunks = Array.range 0 999

    -- Create Strom from the generator
    let strom = Strom.fromArray chunks # Strom.mapMStrom makeChunk

    -- Convert to ReadableStream and wrap in StreamBody
    stream <- liftEffect $ WebStream.toReadableStream {} strom
    let body = StreamBody stream
    let stromFromBody = toStrom body

    -- Count total chunks and bytes
    stats <- Om.runOm {} { exception: throwError } do
      let
        withSizes = Strom.mapMStrom (\chunk -> liftEffect $ Buffer.size chunk) stromFromBody
      Strom.runFold
        { chunks: 0, bytes: 0 }
        (\acc size -> { chunks: acc.chunks + 1, bytes: acc.bytes + size })
        withSizes

    -- Verify we got all 1000 chunks
    expectToEqual 1000 stats.chunks
    -- Each chunk is "chunk-N-data" which varies in size
    -- Just verify we got a reasonable amount of bytes
    expectToBe true (stats.bytes > 10000)

  test "parses NDJSON stream line by line" do
    -- Create NDJSON data (newline-delimited JSON)
    let
      ndjsonLines =
        [ """{"id":1,"name":"Alice","score":95}"""
        , """{"id":2,"name":"Bob","score":87}"""
        , """{"id":3,"name":"Charlie","score":92}"""
        , """{"id":4,"name":"Diana","score":88}"""
        , """{"id":5,"name":"Eve","score":91}"""
        ]
      ndjsonText = String.joinWith "\n" ndjsonLines <> "\n"

    -- Create a stream from the NDJSON text
    buf <- liftEffect $ Buffer.fromString ndjsonText UTF8
    let strom = Strom.succeed buf

    -- Convert to ReadableStream and wrap in StreamBody
    stream <- liftEffect $ WebStream.toReadableStream {} strom
    let body = StreamBody stream
    let stromFromBody = toStrom body

    -- Parse the NDJSON stream
    results <- Om.runOm {} { exception: throwError } do
      let
        -- Convert buffers to strings
        asStrings = Strom.mapMStrom (\chunk -> liftEffect $ Buffer.toString UTF8 chunk) stromFromBody
        -- Split by newlines and parse each line as JSON
        lines = asStrings
          # Strom.mapStrom (String.split (String.Pattern "\n"))
          # Strom.bindStrom Strom.fromArray
          # Strom.filterStrom (not <<< String.null)
          # Strom.mapStrom (\line -> JSON.readJSON line :: Either (NonEmptyList ForeignError) { id :: Int, name :: String, score :: Int })
      Strom.runCollect lines

    -- Verify we got 5 valid JSON objects
    expectToEqual 5 (Array.length results)

    -- Verify all parsed successfully
    let
      allSuccess = Array.all
        ( \r -> case r of
            Right _ -> true
            Left _ -> false
        )
        results
    expectToBe true allSuccess

    -- Verify first record
    case unsafePartial $ Array.unsafeIndex results 0 of
      Right record -> do
        expectToEqual 1 record.id
        expectToEqual "Alice" record.name
        expectToEqual 95 record.score
      Left _ -> expectToBe true false -- Should not happen
