import Html exposing (..)
import Html.Events exposing (..)
import Http
import Random
import Json.Decode exposing (..)
import Time exposing (Time)
import Date
import Task
import Date.Format
import Material
import Material.Scheme
import Material.Card as Card
import Material.Color as Color
import Material.Grid exposing (grid, cell, size, Device(..))

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
    ]
  |> Material.Scheme.top

tile : Html a -> Material.Grid.Cell a
tile card = cell [ size All 4 ] [ card ]

card: Color.Color -> String -> List (Html a) ->  Html a
card c title content =
  Card.view [Color.background c]
  [ Card.title [] [ text title ]
  , Card.text [] content
  ]

-- die view

dieView : Model -> Html Msg
dieView model =
  card (Color.color Color.DeepOrange Color.S400) "Die"
  [ div [] [text (toString model.dieFace)]
  , div [] [button [ onClick RollDie ] [ text "Roll" ]]
  ]

-- price view

priceView : Model -> Html Msg
priceView model =
   card  (Color.color Color.DeepPurple Color.S300) "Bitcoin"
  [ div [] [ text (formatPrice model.price) ]
  , div [] [ text (formatTime model.updateTime) ]
  , div [] [ button [ onClick FetchPrice ] [ text "Refresh" ] ]
  ]

formatPrice: Maybe Float -> String
formatPrice maybePrice =
  maybePrice
  |> Maybe.map (\p -> "$" ++ toString p)
  |> Maybe.withDefault "N/A"

formatTime: Maybe Float -> String
formatTime maybeTime =
  maybeTime
  |> Maybe.map Date.fromTime
  |> Maybe.map (Date.Format.format "%H:%M:%S")
  |> Maybe.map ((++) "Updated at ")
  |> Maybe.withDefault ""
