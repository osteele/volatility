import Html exposing (..)
import Http
import Random
import Json.Decode exposing (..)
import Time exposing (Time)
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
  , updateTime : Maybe Time
  , mdl : Material.Model
  }


init : (Model, Cmd Msg)
init =
  ( Model 0 Nothing Nothing Material.model
  , Cmd.batch [ rollDie, fetchPrice ])

-- messages

type Msg
  = RollDie
  | FetchPrice
  | SetDieFace Int
  | ReceivePrice (Result Http.Error Float)
  | SetPriceTimestamp Time
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
    Mdl msg_ -> Material.update Mdl msg_ model

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- commands

updatePriceTimestamp: Cmd Msg
updatePriceTimestamp =
  Task.perform SetPriceTimestamp Time.now

rollDie : Cmd Msg
rollDie = Random.generate SetDieFace (Random.int 1 6)

fetchPrice : Cmd Msg
fetchPrice =
  let
    url = "https://blockchain.info/ticker"
    decoder = at ["USD"] <| field "last" float
    request = Http.get url decoder
  in Http.send ReceivePrice request

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
  Footer.mini [Color.background Color.white]
    { left =
        Footer.left []
          [ Footer.links []
              [ Footer.linkItem [ Footer.href "https://github.com/osteele/coindie#credits" ] [ Footer.html <| text "Credits"]
              ]
          ]
    , right = Footer.right [] []
    }

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
  card DieCard "Six-Sided Die" Nothing
  <| [
      Options.div [ Typo.display3, Typo.center, Color.text Color.white ] [ text <| toString model.dieFace ]
    , button model 0 "Roll" RollDie
      ]

button: Model -> Int -> String -> Msg -> Html Msg
button model n t onClick =
  Button.render Mdl [n] model.mdl
    [ Button.raised
    , Button.ripple
    , Options.onClick onClick
    ]
    [ text t ]
  |> List.singleton |> Options.div [Typo.right]

-- price view

priceView : Model -> Html Msg
priceView model =
   card PriceCard "Bitcoin" (formatTime model.updateTime)
  <| [
    Options.div [ Typo.display3, Typo.center, Color.text Color.black ] [ text <| formatPrice model.price ]
  , button model 1 "Refresh" FetchPrice
  ]

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
