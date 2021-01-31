module Extra exposing (..)

import Pair


roundBy : Int -> Int -> Int
roundBy unit value =
    let
        q =
            value // unit
    in
    [ q - 1, q, q + 1 ]
        |> List.map (\x -> x * unit)
        |> List.map (\x -> ( x, abs <| x - value ))
        |> List.sortBy Tuple.second
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault value
