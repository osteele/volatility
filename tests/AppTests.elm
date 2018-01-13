module AppTests exposing (..)

import Main
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)


suite : Test
suite =
    describe "App Tests"
        [ test "Correctly Renders Model Content" <|
            \_ ->
                let
                    ( model, _ ) =
                        Main.init
                in
                Main.view model
                    |> Query.fromHtml
                    |> Query.has [ text "Bitcoin" ]
        ]
