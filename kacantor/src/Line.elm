module Line exposing (..)

import Pos exposing (Pos)


type alias Line =
    { p1 : Pos
    , p2 : Pos
    }


type alias Linelike r =
    { r
        | p1 : Pos
        , p2 : Pos
    }


addX : Float -> Pos -> Line
addX x pos =
    pos |> Pos.addX x |> Line pos


addY : Float -> Pos -> Line
addY y pos =
    pos |> Pos.addY y |> Line pos
