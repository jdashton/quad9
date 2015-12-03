module Quad9 where

import Html exposing (div, Html, text)
import Html.Attributes exposing (class)
-- import Html.Events exposing (..)
import StartApp.Simple as StartApp
import List exposing (map)
import Matrix exposing (repeat, Matrix)
  

-- MODEL
  

type alias GameState =
  { grid : Matrix Int }


initialModel : GameState
initialModel =
  { grid = repeat 8 8 0 }


-- VIEW


gridNums : List Int
gridNums = [0..7]


gridCell : a -> Html
gridCell _ =
  div [ class "elm-grid-cell" ] []


gridRow : a -> Html
gridRow _ = 
  div [ class "elm-grid-row" ] (map gridCell gridNums)


defaultBoard : Html
defaultBoard =
  div [ class "elm-grid-container"] (map gridRow gridNums)


view address model =
  div []
    [ defaultBoard
    , div [ class "elm-tile-container" ] [ ]
    ]


-- UPDATE


type Action = Left | Right | Up | Down | Autoplay | Restart | SaveGame


update action model =
  case action of
    Left -> model
    Right -> model
    Up -> model
    Down -> model
    Autoplay -> model
    Restart -> model
    SaveGame -> model


-- MAIN


main =
  StartApp.start { model = initialModel, view = view, update = update }

