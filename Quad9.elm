module Quad9 where

import Html exposing (div, Html, text)
import Html.Attributes exposing (class)
-- import Html.Events exposing (..)
import StartApp.Simple as StartApp
import List exposing (map)
import Matrix exposing (repeat, Matrix, toIndexedArray, set)
import Array exposing (filter, Array, toList)
  

-- MODEL
  

type alias Tile = 
  Int


type alias IndexedTile =
  ( (Int, Int) , Maybe Tile )


type alias GameState =
  { grid : Matrix (Maybe Tile)
  , size : Int
  , moves : Int
  }


initialSize : Int
initialSize = 8


initialModel : GameState
initialModel =
  { grid = repeat initialSize initialSize Nothing
  , size = initialSize
  , moves = 0
  }
    |> addTile ((3, 5), Just (128 * 1024))
    |> addTile ((1, 1), Just (256 * 1024 * 1024))
    |> addTile ((2, 3), Just (512 * 1024 * 1024 * 1024))


emptyTile : IndexedTile -> Bool
emptyTile ((_, _), tile) =
  tile == Nothing


realTile : IndexedTile -> Bool
realTile ((_, _), tile) =
  tile /= Nothing


availableCells : GameState -> Array IndexedTile
availableCells model = 
  model.grid
    |> toIndexedArray
    |> filter emptyTile


addTile : IndexedTile -> GameState -> GameState
addTile ((x, y), tile) model =
  { model | grid = set x y tile model.grid }


-- VIEW


gridCell : a -> Html
gridCell _ =
  div [ class "elm-grid-cell" ] []


gridRow : Int -> a -> Html
gridRow size _ = 
  div [ class "elm-grid-row" ] (map gridCell [1..size])


defaultBoard : Int -> Html
defaultBoard size =
  div [ class "elm-grid-container"] (map (gridRow size) [1..size])


convertNum : Int -> String
convertNum n =
  let
      (o, suf) = reduceNum (n, "")
  in
      abbreviate (o, suf)



abbreviate (n, suf) =
  case n of
    128 -> "⅛" ++ promote suf
    256 -> "¼" ++ promote suf
    512 -> "½" ++ promote suf
    n  -> (toString n) ++ suf
        

promote : String -> String
promote s =
  case s of
    ""  -> "K"
    "K" -> "M"
    "M" -> "G"
    "G" -> "T"
    "T" -> "P"
    "P" -> "E"
    "E" -> "Z"
    "Z" -> "Y"
    _   -> "a lot & a lot"


reduceNum : (Int, String) -> (Int, String)
reduceNum (n, suffix) =
  if n > 512 then reduceNum (n // 1024, promote suffix) else (n, suffix)


-- tile tile-2 tile-position-3-4 tile-new
showTile : IndexedTile -> Html
showTile ((x, y), tile) =
  case tile of
    Just val -> 
      div [ class ("tile" ++ 
                  " tile-" ++ (toString val) ++ 
                  " tile-position-" ++ (toString x) ++ "-" ++ (toString y) ++ 
                  " tile-new") ] 
          [ text (convertNum val) ] 
    Nothing -> text ""


viewTiles : GameState -> Html
viewTiles model =
  div [ class "elm-tile-container" ] 
  (model.grid
    |> toIndexedArray
    |> filter realTile
    |> toList
    |> map showTile
  )


view address model =
  div []
    [ defaultBoard model.size
    , viewTiles model
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

