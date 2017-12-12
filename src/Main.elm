import Html exposing (..)
import Http
import Random
import Json.Decode exposing (..)
import Time exposing (Time, second)
import Char
import Date
import Regex
import Task
import Date.Format
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Footer as Footer
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Options as Options
import Material.Options exposing (Style, css)
import Material.Scheme
import Material.Typography as Typo

main: Program Never Model Msg
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
  , price: Maybe Float
  , curTime: Maybe Time
  , updateTime : Maybe Time
  , mdl : Material.Model
  }


init : (Model, Cmd Msg)
init =
  ( { dieFace = 0
    , price = Nothing
    , curTime = Nothing
    , updateTime = Nothing
    , mdl = Material.model
    }
  , Cmd.batch [ rollDie, fetchPrice ])

-- messages

type Msg
  = RollDie
  | FetchPrice
  | SetDieFace Int
  | ReceivePrice (Result Http.Error Float)
  | SetPriceTimestamp Time
  | Tick Time
  | Mdl (Material.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RollDie -> (model, rollDie)
    SetDieFace face -> ({ model | dieFace = face }, Cmd.none)
    FetchPrice -> (model, fetchPrice)
    ReceivePrice (Ok price) -> ({ model | price = Just price }, updatePriceTimestamp)
    ReceivePrice (Err _) -> (model, Cmd.none)
    SetPriceTimestamp t -> ({ model | updateTime = Just t }, Cmd.none)
    Tick t -> ({ model | curTime = Just t }, Cmd.none)
    Mdl msg_ -> Material.update Mdl msg_ model

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

-- commands

fetchPrice : Cmd Msg
fetchPrice =
  let
    url = "https://blockchain.info/ticker"
    decoder = at ["USD"] <| field "last" float
    request = Http.get url decoder
  in Http.send ReceivePrice request

rollDie : Cmd Msg
rollDie =
  Random.int 1 6
  |> Random.generate SetDieFace

updatePriceTimestamp: Cmd Msg
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
  |> \g -> Html.div [] [ g, footer ]
  |> Material.Scheme.top

caption: Html Msg
caption =
  Options.styled p
  [ Typo.caption ]
  [ text "Two sources of volatility. One sweet page. This is not investment advice." ]

footer: Html Msg
footer =
  let
    url = "https://github.com/osteele/coindie#credits"
  in
    Footer.mini [ Color.background Color.white ]
      { left =
          Footer.left []
            [ Footer.links []
                [ Footer.linkItem
                  [ Footer.href url ]
                  [ Footer.html <| text "Credits"]
                ]
            ]
      , right = Footer.right [] []
      }

button: Model -> Int -> List (Button.Property Msg) -> String -> Msg -> Html Msg
button model index props txt onClick =
  Button.render Mdl [index] model.mdl
    ([ Button.raised
    , Button.ripple
    , Options.onClick onClick
    ] ++ props)
    [ text txt ]
  |> List.singleton |> Options.div [Typo.right]

tile : Html a -> Material.Grid.Cell a
tile card = cell [size All 4] [ card ]

card: CardType -> String -> Maybe String -> List (Html Msg) ->  Html Msg
card cardType title subtitle content =
  Card.view
    [ css "background" <| "url('" ++ (bgImage cardType) ++ "') center / cover"
    , Elevation.e8
    ]
    [ Card.title []
      [ Card.head [] [text title]
      , Card.subhead [Typo.caption] [text <| Maybe.withDefault nbsp subtitle]
      ]
    , Card.text [] content
    ]

-- cards

type CardType = DieCard | PriceCard

bgImage: CardType -> String
bgImage cardType =
  case cardType of
    DieCard -> "assets/dim-die.jpg"
    PriceCard -> "assets/bubble.jpg"

-- die view

dieView: Model -> Html Msg
dieView model =
  [ Options.div
      [ Typo.display3, Typo.center, Color.text Color.white ]
      [ text <| toString model.dieFace ]
  , button model 0 [] "Roll" RollDie
  ]
  |> card DieCard "Six-Sided Die" Nothing

-- price view

priceView : Model -> Html Msg
priceView model =
  let
    elapsed =
      case (model.curTime, model.updateTime) of
        (Just t0, Just t1) -> Just <| (t0 - t1) / 1000
        (_, _) -> Nothing

    remaining =
      elapsed |> Maybe.map (\t -> 15.0 - t ) |> Maybe.map ceiling

    disabled =
      case remaining of
        Just n -> n > 0
        Nothing -> True

    disabledButtonText =
      remaining
      |> Maybe.map (\n -> "Can refresh in " ++ (toString n) ++ pluralize " second" n)
      |> Maybe.withDefault "Waiting for results"

    pluralize noun n =
      if n == 1 then noun else noun ++ "s"

    buttonProps =
      if disabled then [ Button.disabled ] else []

    buttonTitle =
      if disabled then "[" ++ disabledButtonText ++ "…]" else "Refresh"

    subtitle = formatTime model.updateTime
  in
    [ Options.div [ Typo.display3, Typo.center, Color.text Color.black ] [ text <| formatPrice model.price ]
    , button model 1 buttonProps buttonTitle FetchPrice
    ]
    |> card PriceCard "Bitcoin" subtitle

-- formatters

{-| Insert thousands separators in an number string -}
addCommas: number -> String
addCommas n =
  toString n
  |> String.reverse
  |> Regex.find Regex.All (Regex.regex "(\\d*\\.)?\\d{0,3}")
  |> List.map (\m -> m.match)
  |> String.join ","
  |> String.reverse

formatPrice: Maybe Float -> String
formatPrice maybePrice =
  maybePrice
  |> Maybe.map addCommas
  |> Maybe.map ((++) "$")
  |> Maybe.withDefault "N/A"

formatTime: Maybe Float -> Maybe String
formatTime maybeTime =
  maybeTime
  |> Maybe.map Date.fromTime
  |> Maybe.map (Date.Format.format "%H:%M:%S")
  |> Maybe.map ((++) "Updated at ")

{-| Non-printing string, for aligning cells -}
nbsp: String
nbsp = Char.fromCode 0x00A0 |> String.fromChar
