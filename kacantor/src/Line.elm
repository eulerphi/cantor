module Line exposing (Line, Linelike, addX, addXY, addY, centeredX, centeredY, midPos, toX, toY)

import Box exposing (Boxlike)
import OffsetAnchor exposing (OffsetAnchor)
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


centeredX : Float -> Pos -> Line
centeredX x pos =
    Line (pos |> Pos.addX x) (pos |> Pos.addX -x)


centeredY : Float -> Pos -> Line
centeredY y pos =
    Line (pos |> Pos.addY y) (pos |> Pos.addY -y)


midPos : Line -> Pos
midPos line =
    Pos (line.p2.x - line.p1.x) (line.p2.y - line.p1.y)
        |> Pos.scale 0.5
        |> Pos.add line.p1


toX : Float -> Pos -> Line
toX x pos =
    pos |> Pos.updateX x |> Line pos


toY : Float -> Pos -> Line
toY y pos =
    pos |> Pos.updateY y |> Line pos
