module Main exposing (..)

import Char
import Currency exposing (..)
import Date
import Date.Format
import Html exposing (..)
import Http
import Json.Decode exposing (..)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Footer as Footer
import Material.Grid exposing (Device(..), cell, grid, size)
import Material.Options as Options exposing (Style, css)
import Material.Scheme
import Material.Typography as Typo
import Random
import Task
import Time exposing (Time, second)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- model


type alias Model =
    { dieFace : Int
    , lastPrice : Maybe Float
    , price : Maybe Float
    , curTime : Maybe Time
    , updateTime : Maybe Time
    , mdl : Material.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { dieFace = 0
      , lastPrice = Nothing
      , price = Nothing
      , curTime = Nothing
      , updateTime = Nothing
      , mdl = Material.model
      }
    , Cmd.batch [ rollDie, fetchPrice ]
    )



-- messages


type Msg
    = RollDie
    | SetDieFace Int
    | FetchPrice
    | ReceivePrice (Result Http.Error Float)
    | SetPriceTimestamp Time
    | Tick Time
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDie ->
            ( model, rollDie )

        SetDieFace face ->
            ( { model | dieFace = face }, Cmd.none )

        FetchPrice ->
            ( model, fetchPrice )

        ReceivePrice (Ok price) ->
            ( { model | price = Just price, lastPrice = model.price }, updatePriceTimestamp )

        ReceivePrice (Err _) ->
            ( model, Cmd.none )

        SetPriceTimestamp t ->
            ( { model | updateTime = Just t }, Cmd.none )

        Tick t ->
            ( { model | curTime = Just t }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- commands


fetchPrice : Cmd Msg
fetchPrice =
    let
        url =
            "https://blockchain.info/ticker"

        decoder =
            at [ "USD" ] <| field "last" float

        request =
            Http.get url decoder
    in
    Http.send ReceivePrice request


rollDie : Cmd Msg
rollDie =
    Random.int 1 6
        |> Random.generate SetDieFace


updatePriceTimestamp : Cmd Msg
updatePriceTimestamp =
    Task.perform SetPriceTimestamp Time.now



-- views


view : Model -> Html Msg
view model =
    grid []
        [ tile <| dieView model
        , tile <| priceView model
        , cell [ size All 12 ] [ caption ]
        ]
        |> (\g ->
                Html.div [] [ g, footer ]
                    |> Material.Scheme.top
           )


caption : Html Msg
caption =
    Options.styled p
        [ Typo.caption ]
        [ text "Two sources of volatility. One sweet page. This is not investment advice." ]


footer : Html Msg
footer =
    let
        url =
            "https://github.com/osteele/coindie#credits"
    in
    Footer.mini [ Color.background Color.white ]
        { left =
            Footer.left []
                [ Footer.links []
                    [ Footer.linkItem
                        [ Footer.href url ]
                        [ Footer.html <| text "Credits" ]
                    ]
                ]
        , right = Footer.right [] []
        }


button : Model -> Int -> List (Button.Property Msg) -> String -> Msg -> Html Msg
button model index props txt onClick =
    Button.render Mdl
        [ index ]
        model.mdl
        ([ Button.raised
         , Button.ripple
         , Options.onClick onClick
         ]
            ++ props
        )
        [ text txt ]
        |> List.singleton
        |> Options.div [ Typo.right ]


tile : Html a -> Material.Grid.Cell a
tile card =
    cell [ size All 4 ] [ card ]


card : CardType -> String -> Maybe String -> List (Html Msg) -> Html Msg
card cardType title subtitle content =
    Card.view
        [ css "background" <| "url('" ++ bgImage cardType ++ "') center / cover"
        , Elevation.e8
        ]
        [ Card.title []
            [ Card.head [] [ text title ]
            , Card.subhead [ Typo.caption ] [ text <| Maybe.withDefault nbsp subtitle ]
            ]
        , Card.text [] content
        ]



-- cards


type CardType
    = DieCard
    | PriceCard


bgImage : CardType -> String
bgImage cardType =
    case cardType of
        DieCard ->
            "assets/dim-die.jpg"

        PriceCard ->
            "assets/bubble.jpg"



-- die view


dieView : Model -> Html Msg
dieView model =
    [ Options.div
        [ Typo.display3, Typo.center, Color.text Color.white ]
        [ text <| toString model.dieFace ]
    , Options.div [ Typo.display1 ] [ text nbsp ]
    , button model 0 [] "Roll" RollDie
    ]
        |> card DieCard "Six-Sided Die" Nothing



-- price view


priceView : Model -> Html Msg
priceView model =
    let
        subtitle =
            model.updateTime
                |> Maybe.map formatTime
                |> Maybe.map ((++) "Updated at ")

        elapsedTime =
            Maybe.map2 (-) model.curTime model.updateTime
                |> Maybe.map (flip (/) 1000)

        remainingTime =
            elapsedTime |> Maybe.map ((-) 15.0) |> Maybe.map ceiling

        disabled =
            case remainingTime of
                Just n ->
                    n > 0

                Nothing ->
                    True

        disabledButtonText =
            remainingTime
                |> Maybe.map (\n -> "Refreshable in " ++ toString n ++ pluralize " second" n)
                |> Maybe.withDefault "Waiting for results"

        buttonProps =
            if disabled then
                [ Button.disabled ]
            else
                []

        buttonTitle =
            if disabled then
                "[" ++ disabledButtonText ++ "…]"
            else
                "Refresh"

        priceDelta =
            Maybe.map2 (-) model.price model.lastPrice

        priceDeltaColor =
            case priceDelta of
                Just d ->
                    if d >= 0 then
                        Color.color Color.Green Color.S900
                    else
                        Color.color Color.Red Color.S900

                Nothing ->
                    Color.black

        priceDeltaText =
            let
                fmt p =
                    let
                        s =
                            toDecimal 2 p

                        zero =
                            toDecimal 2 0
                    in
                    if s == zero then
                        "no change!"
                    else if p > 0 then
                        "+" ++ s
                    else
                        s
            in
            priceDelta
                |> Maybe.map fmt
                |> Maybe.withDefault nbsp
    in
    [ Options.div [ Typo.display3, Typo.center, Color.text Color.black ]
        [ text <| Maybe.withDefault "NA" <| Maybe.map (toPrice "$") <| model.price ]
    , Options.div [ Typo.display1, Typo.right, Color.text priceDeltaColor ] [ text <| priceDeltaText ]
    , button model 1 buttonProps buttonTitle FetchPrice
    ]
        |> card PriceCard "Bitcoin" subtitle



-- formatters


formatTime : Time -> String
formatTime =
    Date.fromTime >> Date.Format.format "%H:%M:%S"


{-| Non-printing string, for aligning cells
-}
nbsp : String
nbsp =
    Char.fromCode 0xA0 |> String.fromChar


pluralize : String -> Int -> String
pluralize noun n =
    if n == 1 then
        noun
    else
        noun ++ "s"
