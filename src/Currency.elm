module Currency exposing (..)

import Char
import Regex
import String

{-| Insert thousands separators in an number string -}
addCommas: String -> String
addCommas s =
  s
  |> String.reverse
  |> Regex.find Regex.All (Regex.regex "(\\d*\\.)?\\d{0,3}-?")
  |> List.map .match
  |> String.join ","
  |> String.reverse


toDecimal: Int -> Float -> String
toDecimal prec num =
  let
    decimal: String
    decimal =
      case toString num |> split2 "e" of
        (mantissa, Just exp) ->
          expToDecimal (mantissa, exp)
        (s, Nothing) ->
          s

    padded: String
    padded =
      case split2 "." decimal of
        (whole, Just frac) ->
          whole ++ "." ++ frac ++ (zeros <| max 0 <| prec - String.length frac)
        (whole, Nothing) ->
          whole ++ "." ++ zeros prec

    expToDecimal (mantissa, exp) =
      let
        zs = String.toInt exp |> Result.withDefault 0 |> abs |> zeros
      in
        if String.startsWith exp "-" then
          "0." ++ zs ++ mantissa
        else
          mantissa ++ zs

    split2 sep s =
      case String.split sep s of
        [] ->
          (s, Nothing)
        a :: [] ->
          (a, Nothing)
        a :: b :: cs ->
          (a, Just <| String.join sep <| b :: cs)

    -- n-length string of zeros
    zeros: Int -> String
    zeros n =
      List.repeat n '0' |> String.fromList

    insertDecimal: Int -> String -> String
    insertDecimal prec digits =
      let
        n = String.length digits
        whole = String.left (n - prec) digits
        frac = String.right prec digits
      in
        whole ++ "." ++ frac

    removeDigits: Int -> List Int -> List Int
    removeDigits remove digits =
      let
        round1 remove carry digits =
          case digits of
            [] ->
              if carry > 0 then [carry] else []
            d :: ds ->
              if remove > 0 then
                round1 (remove - 1) (if d >= 5 then 1 else 0) ds
              else
                let
                  d_ = d + carry
                in
                  d_ % 10 :: round1 (remove - 1) (d_ // 10) ds
      in
        round1 remove 0 digits

  in
    if num < 0 then
      "-" ++ (toDecimal prec -num)
    else
      case split2 "." <| padded of
        (whole, Just frac) ->
          whole ++ frac
          |> String.toList
          |> List.reverse
          |> List.map (\c -> Char.toCode c - Char.toCode '0')
          |> removeDigits ((String.length frac) - prec)
          |> List.map (\n -> Char.fromCode (n + Char.toCode '0'))
          |> List.reverse
          |> String.fromList
          |> insertDecimal prec

        (s, Nothing) ->
          s

toPrice: String -> Float -> String
toPrice symbol price =
  price
  |> toDecimal 2
  |> addCommas
  |> (++) symbol
