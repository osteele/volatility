module Tests exposing (..)

import Array
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

    , describe "formatDecimal"
      -- FIXME places = 0
      [ test "rounds positive numbers"
        <| \_ ->
            formatDecimal 2 123.456
            |> Expect.equal "123.46"
      , test "rounds negative numbers"
        <| \_ ->
            formatDecimal 2 -123.456
            |> Expect.equal "-123.46"
      , test "rounds into the whole place"
        <| \_ ->
            formatDecimal 2 123.999
            |> Expect.equal "124.00"
      , test "pads with zeros"
        <| \_ ->
            formatDecimal 2 123.0
            |> Expect.equal "123.00"
      , fuzz2 (Fuzz.intRange 1 4) fuzzFloat "includes places after the decimal"
        <| \places number ->
              let
                s = formatDecimal places number
              in
                String.split "." s |> Array.fromList |> Array.get 1 |> Maybe.map String.length
                |> Expect.equal (Maybe.Just places)
      ]

    , describe "formatPrice"
      [ test "includes decimals"
        <| \_ ->
            formatPrice "$" 123.4
            |> Expect.equal "$123.40"
      , test "works with negative numbers"
        <| \_ ->
          formatPrice "$" -123.4
          |> Expect.equal "$-123.40"
      ]
    ]


fuzzFloat: Fuzzer Float
fuzzFloat =
  Fuzz.map toFloat Fuzz.int
