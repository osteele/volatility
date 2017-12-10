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
  (Model 0 Nothing Nothing Material.model, Cmd.batch [ rollDie, fetchPrice, getTime ])

-- messages

type Msg
  = RollDie
  | FetchPrice
  -- | FetchError Http.Error
  -- | FetchSuccess String
  | NewFace Int
  | NewPrice Float
  | ReceivePrice (Result Http.Error Float)
  | OnTime Time
  | Mdl (Material.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RollDie -> (model, rollDie)
    NewFace face -> ({ model | dieFace = face }, Cmd.none)
    FetchPrice -> model ! [ fetchPrice, getTime ]
    NewPrice price -> ({ model | price = Just price }, Cmd.none)
    ReceivePrice (Ok price) -> ({ model | price = Just price }, Cmd.none)
    ReceivePrice (Err _) -> (model, Cmd.none)
    OnTime t -> ({ model | updateTime = Just t }, Cmd.none)
    Mdl msg_ -> Material.update Mdl msg_ model

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- commands

getTime: Cmd Msg
getTime =
    Time.now
        |> Task.perform OnTime

rollDie : Cmd Msg
rollDie = Random.generate NewFace (Random.int 1 6)

priceDecoder : Decoder Float
priceDecoder =
  (at ["USD"] (field "last" float))

fetchPrice : Cmd Msg
fetchPrice =
    let
      url = "https://blockchain.info/ticker"
      request = Http.get url priceDecoder
    in Http.send ReceivePrice request
    -- in Task.perform FetchError FetchSuccess request

-- views

view : Model -> Html Msg
view model =
  grid []
    [ cell [ size All 4 ] [dieView model]
    , cell [ size All 4 ] [priceView model]
    ]
  |> Material.Scheme.top


dieView : Model -> Html Msg
dieView model =
  Card.view [Color.background (Color.color Color.DeepOrange Color.S400)]
  [ Card.title [] [ text "Die" ]
  , Card.text [] [
      div [] [text (toString model.dieFace)]
    , div [] [button [ onClick RollDie ] [ text "Roll" ]]
    ]
  ]

priceView : Model -> Html Msg
priceView model =
  Card.view [Color.background (Color.color Color.DeepPurple Color.S300)]
  [ Card.title [] [ text "Bitcoin" ]
  , Card.text [] [
    div [] [ text (formatPrice model.price) ]
  , div [] [ text (formatTime model.updateTime) ]
  , div [] [ button [ onClick FetchPrice ] [ text "Refresh" ] ]
  ]]

formatPrice: Maybe Float -> String
formatPrice maybePrice =
  case maybePrice of
    Nothing -> "N/A"
    Just price -> "$" ++ toString price

formatTime: Maybe Float -> String
formatTime maybeTime =
  case maybeTime of
    Nothing -> ""
    Just time -> "Updated at " ++ Date.Format.format "%H:%M:%S" (Date.fromTime time)
