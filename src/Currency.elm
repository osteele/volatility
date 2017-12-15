module Currency exposing (..)
import Regex

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
toDecimal places num =
  let
    m =
      10^places

    n =
      round ((toFloat m) * num)

    whole =
      toString <| n // m

    frac =
      toString <| n % m

    padding =
      List.repeat (places - String.length frac) '0' |> String.fromList
  in
    if num < 0
        then "-" ++ (toDecimal places -num)
        else whole ++ "." ++ padding ++ frac

toPrice: String -> Float -> String
toPrice symbol price =
  price
  |> toDecimal 2
  |> addCommas
  |> (++) symbol
