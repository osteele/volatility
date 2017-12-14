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


formatDecimal: Int -> Float -> String
formatDecimal places num =
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
        then "-" ++ (formatDecimal places -num)
        else whole ++ "." ++ frac ++ padding

formatPrice: String -> Float -> String
formatPrice symbol price =
  price
  |> formatDecimal 2
  |> addCommas
  |> (++) symbol
