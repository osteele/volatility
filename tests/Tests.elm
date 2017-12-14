module Tests exposing (..)

import Array
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
      -- FIXME doesn't work with negative numbers
      -- , fuzz int "parse"
      --   <| \num -> Expect.equal (addCommas <| toString num) (toString num)
      ]
    , describe "formatDecimal"
      -- FIXME places = 0
      [ test "rounds positive numbers"
        <| \() -> Expect.equal (formatDecimal 2 123.456) "123.46"
      -- FIXME
      -- , test "rounds negative numbers"
      --   <| \() -> Expect.equal (formatDecimal 2 -123.456) "-123.47"
      , test "pads with zeros"
        <| \() -> Expect.equal (formatDecimal 2 123.0) "123.00"
      , fuzz2 (Fuzz.intRange 1 4) (Fuzz.map toFloat Fuzz.int) "includes places after the decimal"
        <| \places number ->
              let s = formatDecimal places number
              in Expect.equal (String.split "." s |> Array.fromList |> Array.get 1 |> Maybe.map String.length) (Maybe.Just places)
      ]
    , describe "formatPrice"
      [ test "includes decimals"
        <| \() -> Expect.equal (formatPrice "$" 123.4) "$123.40"
      -- FIXME
      -- , test "works with negative numbers"
      --   <| \() -> Expect.equal (formatPrice "$" -123.4) "$-123.40"
      ]
    ]
