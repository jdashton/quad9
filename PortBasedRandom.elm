module PortBasedRandom where

import Mouse
import Signal exposing (Signal, map)
import Random exposing (Seed)
import Graphics.Element exposing (Element, show)

port primer : Float


firstSeed : Seed
firstSeed =
  Random.initialSeed <| round primer


type alias Model =
  { nextSeed : Seed 
  , currentInt : Int
  }


initialModel : Model
initialModel =
  { nextSeed = firstSeed
  , currentInt = 0
  }


randomInt : Model -> Model
randomInt model = 
  let
      (i, s) = Random.generate (Random.int 1 10) model.nextSeed
  in
      { model | nextSeed = s, currentInt = i }


update : (Int, Int) -> Model -> Model
update (_, _) model =
  randomInt model


main : Signal Element
main =
  Signal.foldp update initialModel Mouse.position
    |> map (\m -> show m.currentInt)
