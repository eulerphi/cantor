module Line exposing (Line, Linelike, addX, addXY, addY)

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


addXY : Float -> Float -> Pos -> Line
addXY x y pos =
    pos |> Pos.addX x |> Pos.addY y |> Line pos


addY : Float -> Pos -> Line
addY y pos =
    pos |> Pos.addY y |> Line pos
