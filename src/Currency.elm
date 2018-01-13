module Currency exposing (..)

import Char
import Regex
import String


{-| Insert thousands separators in a number string
-}
insertCommas : String -> String
insertCommas =
    String.reverse
        >> Regex.find Regex.All (Regex.regex "(\\d*\\.)?\\d{0,3}-?")
        >> List.map .match
        >> String.join ","
        >> String.reverse



{- Round to prec decimals. Rounds half away from zero. -}


toDecimal : Int -> Float -> String
toDecimal prec num =
    let
        decimal : String
        decimal =
            case toString num |> split2 "e" of
                ( mantissa, Just exp ) ->
                    expToDecimal ( mantissa, exp )

                ( s, Nothing ) ->
                    s

        padded : String
        padded =
            case split2 "." decimal of
                ( whole, Just frac ) ->
                    whole ++ "." ++ frac ++ (zeros <| max 0 <| prec - String.length frac)

                ( whole, Nothing ) ->
                    whole ++ "." ++ zeros prec

        expToDecimal ( mantissa, exp ) =
            let
                zs =
                    String.toInt exp |> Result.withDefault 0 |> abs |> zeros
            in
            if String.startsWith exp "-" then
                "0." ++ zs ++ mantissa
            else
                mantissa ++ zs

        split2 : String -> String -> ( String, Maybe String )
        split2 sep s =
            case String.indices sep s of
                [] ->
                    ( s, Nothing )

                ix :: _ ->
                    ( String.left ix s, Just <| flip String.dropLeft s <| ix + String.length sep )

        -- n-length string of zeros
        zeros : Int -> String
        zeros n =
            List.repeat n '0' |> String.fromList

        -- insert a decimal prec digits from the right
        insertDecimal : Int -> String -> String
        insertDecimal prec digits =
            let
                len =
                    String.length digits

                whole =
                    String.left (len - prec) digits

                frac =
                    String.right prec digits
            in
            whole ++ "." ++ frac

        -- remove the first n digits, rounding half away from zero
        removeDigits : Int -> List Int -> List Int
        removeDigits n digits =
            let
                round1 n carry digits =
                    case digits of
                        [] ->
                            if carry > 0 then
                                [ carry ]
                            else
                                []

                        d :: ds ->
                            let
                                remaining carry =
                                    round1 (n - 1) carry ds

                                d_ =
                                    d + carry
                            in
                            if n > 0 then
                                remaining <|
                                    if d >= 5 then
                                        1
                                    else
                                        0
                            else
                                d_ % 10 :: remaining (d_ // 10)
            in
            round1 n 0 digits
    in
    if num < 0 then
        "-" ++ toDecimal prec -num
    else
        case split2 "." padded of
            ( whole, Just frac ) ->
                whole
                    ++ frac
                    |> String.toList
                    |> List.reverse
                    |> List.map (\c -> Char.toCode c - Char.toCode '0')
                    |> removeDigits (String.length frac - prec)
                    |> List.map (\n -> Char.fromCode (n + Char.toCode '0'))
                    |> List.reverse
                    |> String.fromList
                    |> insertDecimal prec

            ( s, Nothing ) ->
                s



{- Display as a currency, prefixed with symbol and with two digits precision. Rounds half away from zero. -}


toPrice : String -> Float -> String
toPrice symbol =
    toDecimal 2
        >> insertCommas
        >> (++) symbol
