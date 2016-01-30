module Quad9 where

import Html exposing (div, Html, text)
import Html.Attributes exposing (class)
-- import Html.Events exposing (..)
import StartApp
import List exposing (map)
import Matrix exposing (repeat, Matrix, toIndexedArray, set)
import Array exposing (filter, Array, toList, get, length)
import Random exposing (Seed)
import Keyboard exposing (arrows)
import Effects exposing (Effects, Never)
import Task exposing (Task)
  

-- MODEL

port primer : Int
  

type alias Tile = 
  Int


type alias IndexedTile =
  ( (Int, Int) , Maybe Tile )


type alias GameState =
  { grid  : Matrix (Maybe Tile)
  , size  : Int
  , moves : Int
  , seed  : Seed
  }


initialSize : Int
initialSize = 8


initialModel : GameState
initialModel =
  { grid = repeat initialSize initialSize Nothing
  , size = initialSize
  , moves = 0
  , seed = Random.initialSeed primer
  }
    |> addRandomTile -- ((3, 5), Just (128 * 1024))
    |> addRandomTile -- ((1, 1), Just (256 * 1024 * 1024))
    -- |> addTile ((2, 3), Just (512 * 1024 * 1024 * 1024))


init : (GameState, Effects Action)
init =
  (initialModel, Effects.none)


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


addRandomTile : GameState -> GameState
addRandomTile model =
  let
      cells = availableCells model
      (i, s) = Random.generate (Random.int 0 <| (length cells) - 1) model.seed
      pos = case get i cells of
        Just (p, _) -> p
        Nothing -> (7, 7)
      (v, s') = Random.generate (Random.int 1 10) s
      tileVal = if v > 9 then 4 else 2
  in
      { model | seed = s' }
      |> addTile (pos, Just tileVal)


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
-- x and y in the CSS are 1-based, not 0-based
showTile : IndexedTile -> Html
showTile ((x, y), tile) =
  case tile of
    Just val -> 
      div [ class ("tile" ++ 
                  " tile-" ++ (toString val) ++ 
                  " tile-position-" ++ (toString (x + 1)) ++ "-" ++ (toString (y + 1)) ++ 
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


update : Action -> GameState -> (GameState, Effects Action)
update action model =
  case action of
    Left -> (model |> addRandomTile, Effects.none)
    Right -> (model, Effects.none)
    Up -> (model, Effects.none)
    Down -> (model, Effects.none)
    Autoplay -> (model, Effects.none)
    Restart -> (model, Effects.none)
    SaveGame -> (model, Effects.none)


-- INPUTS
inputs : List (Signal { x : Int, y : Int })
inputs =
  [Signal.map keysToAction Keyboard.keysDown]


keysToAction : { Int, Int } -> Action
keysToAction {x, y} =
  case x of
    1 -> 


port tasks : Signal (Task Never ())
port tasks =
  app.tasks


-- MAIN

app =
  StartApp.start 
    { init   = init
    , update = update
    , view   = view
    , inputs = []
    }

main : Signal Html
main =
  app.html


----------------------------
-- implement workaround for Keyboard.presses
type alias KeyboardState = { temp : Set KeyCode
                           , actualKey : Maybe KeyCode
                           }
keyboardState = { temp = Set.empty
                , actualKey = Nothing
                }


keyboardStep : Set KeyCode -> KeyboardState -> KeyboardState
keyboardStep newKeys state =
  let
    actualKey = List.head <| Set.toList <| newKeys `Set.diff` state.temp
  in
    { state |
      temp <- newKeys
    , actualKey <- actualKey
    }


presses : Signal (Maybe KeyCode)
presses = (\state -> state.actualKey)
          <~ Signal.foldp keyboardStep keyboardState keysDown

