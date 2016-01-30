-- correct roguelike movement by using a workaround for Keyboard

-- See https://github.com/elm-lang/core/issues/326 for discussion

import Color
import Graphics.Collage exposing (Form, text, collage)
import Graphics.Element exposing (Element, container, middle)
import Keyboard exposing (keysDown)
import Char exposing (KeyCode)
import Set exposing (Set)
import Signal exposing (foldp, map, map2)
import Text
import Window


-- CONFIG
screenSize = 20

-- HELPERS

-- MODEL
type alias Player = { x : Int
                    , y : Int
                    }
defPlayer = { x = screenSize//2
            , y = screenSize//2
            }

-- UPDATE
-- 37,38,39,40 ~ left,up,right,down
stepPlayer : Maybe KeyCode -> Player -> Player
stepPlayer key player =
  case key of
    Nothing -> player
    Just key ->
      if      key == 37 then { player | x = player.x - 1, y = player.y + 0 }
      else if key == 38 then { player | x = player.x + 0, y = player.y - 1 }
      else if key == 39 then { player | x = player.x + 1, y = player.y + 0 }
      else if key == 40 then { player | x = player.x + 0, y = player.y + 1 }
      else                     player


-- VIEW
-- recalibrate collage (center to left-upper)
move : (Int, Int) -> (Int, Int) -> Form -> Form
move (w, h) (x, y) form =
  let
    w' = toFloat w
    h' = toFloat h
    x' = toFloat x
    y' = toFloat y
    screenSize' = toFloat screenSize
    width = w'/screenSize'
    height = h'/screenSize'
    newX = -w'/2 + x'*width  + width /2
    newY =  h'/2 - y'*height - height/2
  in
    Graphics.Collage.move (newX, newY) form

view : (Int, Int) -> Player -> Element
view (w,h) {x,y} =
  let
    s = min w h -- collageSize
    playerForm = "@"
               |> Text.fromString
               |> Text.height ((toFloat s) / screenSize)
               |> text
    forms = [ move (s, s) (x, y) playerForm ]
  in
    container w h middle
       <| collage s s forms


-- MAIN
main : Signal Element
-- main = view <~ Window.dimensions ~ Signal.foldp stepPlayer defPlayer Keyboard.arrows
main = map2 view Window.dimensions (foldp stepPlayer defPlayer presses)

----------------------------
-- implement workaround for Keyboard.presses
type alias KeyboardState =
 { temp : Set KeyCode
 , actualKey : Maybe KeyCode
 }


keyboardState =
  { temp = Set.empty
  , actualKey = Nothing
  }


keyboardStep : Set KeyCode -> KeyboardState -> KeyboardState
keyboardStep newKeys state =
  let
    actualKey = List.head <| Set.toList <| newKeys `Set.diff` state.temp
  in
    { state
    | temp = newKeys
    , actualKey = actualKey
    }


presses : Signal (Maybe KeyCode)
presses =
  map (\state -> state.actualKey) (foldp keyboardStep keyboardState keysDown)

