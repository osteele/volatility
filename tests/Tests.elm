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
    [ describe "insertCommas"
      [ test "inserts commas"
          <| \_ ->
            insertCommas "1234"
            |> Expect.equal "1,234"
      , test "inserts multiple commas"
          <| \_ ->
            insertCommas "1234567"
            |> Expect.equal "1,234,567"
      , test "preserves short strings"
          <| \_ ->
            insertCommas "123"
            |> Expect.equal "123"
      , test "preserves decimals in short strings"
          <| \_ ->
            insertCommas "123.45"
            |> Expect.equal "123.45"
      , test "preserves decimals in longer strings"
          <| \_ ->
            insertCommas "1234.56"
            |> Expect.equal "1,234.56"
      , fuzz int "is equivalent to toString, once commas are stripped"
        <| \num ->
          let
            s = toString num
          in
            Expect.equal (String.Extra.replace "," "" <| insertCommas s) s
      ]

    , describe "toDecimal"
      [ test "rounds positive numbers"
        <| \_ ->
            toDecimal 2 123.456
            |> Expect.equal "123.46"
      , test "rounds away from zero"
        <| \_ ->
            [1.01, 1.04, 1.045, 1.049, 1.05, 1.051, 1.09]
            |> List.map (toDecimal 1)
            |> Expect.equal ["1.0", "1.0", "1.0", "1.0", "1.1", "1.1", "1.1"]
      , test "rounds negative numbers away from zero"
        <| \_ ->
            [-1.01, -1.049, -1.05, -1.09]
            |> List.map (toDecimal 1)
            |> Expect.equal ["-1.0", "-1.0", "-1.1", "-1.1"]
      , test "rounds into the integer portion"
        <| \_ ->
            toDecimal 2 123.999
            |> Expect.equal "124.00"
      , test "pads with zeros"
        <| \_ ->
            [123.0, 123.4]
            |> List.map (toDecimal 2)
            |> Expect.equal ["123.00", "123.40"]
      , test "handles zero precision"
        <| \_ ->
            toDecimal 0 123.0
            |> Expect.equal "123."
      , test "works on integers"
        <| \_ ->
            toDecimal 2 123
            |> Expect.equal "123.00"
      , fuzz2 fuzzPrecision fuzzDecimal "includes the correct number of places after the decimal"
        <| \prec num ->
            toDecimal prec num
            |> String.split "."
            |> Array.fromList
            |> Array.get 1
            |> Maybe.map String.length
            |> Expect.equal (Maybe.Just prec)
      , fuzz2 fuzzPrecision fuzzDecimal "is correct to within precision decimals"
        <| \prec num ->
            let
              tolerance = 0.5 / (10 ^ toFloat prec)
            in
              toDecimal prec num
              |> String.toFloat
              |> withOk (Expect.within (Expect.Absolute tolerance) num)
      ]

    , describe "toPrice"
      [ test "includes decimals"
        <| \_ ->
            toPrice "$" 123.4
            |> Expect.equal "$123.40"
      , test "rounds"
        <| \_ ->
            toPrice "$" 123.456
            |> Expect.equal "$123.46"
      , test "adds commas"
        <| \_ ->
            toPrice "$" 1234.5
            |> Expect.equal "$1,234.50"
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
