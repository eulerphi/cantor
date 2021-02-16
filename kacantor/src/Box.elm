module Box exposing (Box, addPos, fromInt, hasSize, scale)

import Pos exposing (Pos)
import Size exposing (Size)


type alias Box =
    { pos : Pos
    , size : Size
    }


addPos : Pos -> Box -> Box
addPos pos box =
    { box | pos = Pos.add pos box.pos }


fromInt : { x : Int, y : Int, width : Int, height : Int } -> Box
fromInt input =
    Box (Pos.fromInt ( input.x, input.y )) (Size.fromInt ( input.width, input.height ))


hasSize : Box -> Bool
hasSize box =
    box.size.width > 0 && box.size.height > 0


scale : Float -> Box -> Box
scale value box =
    Box (box.pos |> Pos.scale value) (box.size |> Size.scale value)
