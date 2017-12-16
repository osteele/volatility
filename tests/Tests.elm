module Tests exposing (..)

import Array
import Random
import String.Extra
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Currency exposing (..)


suite : Test
suite =
  describe "currency functions"
    [ describe "addCommas"
      [ test "inserts commas"
        <| \() -> Expect.equal (addCommas "1234") "1,234"
      , test "inserts multiple commas"
        <| \() -> Expect.equal (addCommas "1234567") "1,234,567"
      , test "does nothing to short strings"
        <| \() -> Expect.equal (addCommas "123") "123"
      , test "preserves decimals"
        <| \() -> Expect.equal (addCommas "123.45") "123.45"
      , test "preserves decimals (2)"
        <| \() -> Expect.equal (addCommas "1234.56") "1,234.56"
      , fuzz int "is equivalent to toString once commas are stripped"
        <| \num ->
          let
            s = toString num
          in
            Expect.equal (String.Extra.replace "," "" <| addCommas s) s
      ]

    , describe "toDecimal"
      [ test "rounds positive numbers"
        <| \_ ->
            toDecimal 2 123.456
            |> Expect.equal "123.46"
      , test "pads positive numbers"
        <| \_ ->
            toDecimal 2 123.4
            |> Expect.equal "123.40"
      , test "rounds only the last digit"
        <| \_ ->
            toDecimal 1 1.249
            |> Expect.equal "1.2"
      , test "rounds negative numbers"
        <| \_ ->
            toDecimal 2 -123.456
            |> Expect.equal "-123.46"
      , test "rounds into the whole place"
        <| \_ ->
            toDecimal 2 123.999
            |> Expect.equal "124.00"
      , test "pads with zeros"
        <| \_ ->
            toDecimal 2 123.0
            |> Expect.equal "123.00"
      , test "handles zero precision"
        <| \_ ->
            toDecimal 0 123.0
            |> Expect.equal "123."
      , fuzz2 fuzzPrecision fuzzDecimal "includes the correct number of places after the decimal"
        <| \places num ->
              let
                s = toDecimal places num
              in
                String.split "." s |> Array.fromList |> Array.get 1 |> Maybe.map String.length
                |> Expect.equal (Maybe.Just places)
      , fuzz2 fuzzPrecision fuzzDecimal "is correct to within precision decimals"
        <| \places num ->
             toDecimal places num |> String.toFloat
              |> withOk
                (Expect.within (places |> toFloat |> (/) 0.1 |> Expect.Absolute) num)
      ]

    , describe "toPrice"
      [ test "includes decimals"
        <| \_ ->
            toPrice "$" 123.4
            |> Expect.equal "$123.40"
      , test "works with negative numbers"
        <| \_ ->
          toPrice "$" -123.4
          |> Expect.equal "$-123.40"
      ]
    ]


fuzzPrecision: Fuzzer Int
fuzzPrecision =
  Fuzz.intRange 0 10

fuzzDecimal: Fuzzer Float
fuzzDecimal =
  Fuzz.float

withOk: (a -> Expectation) -> Result String a -> Expectation
withOk fn result =
  case result of
    Result.Err s ->
      Expect.fail s
    Result.Ok value ->
      fn value
