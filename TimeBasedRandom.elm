module TimeBasedRandom where

import Random

import Time exposing (Time)
import Signal exposing (Signal, map)
import Graphics.Element exposing (show)

seed : Signal Random.Seed
seed = map (\ (t, _) -> Random.initialSeed <| round t) (Time.timestamp (Signal.constant ()))

randomList : Random.Seed -> List Int
randomList seed = 
    let (ls, _) = Random.generate (Random.list 20 (Random.int 0 100)) seed
    in ls

main = map show (map randomList seed)

