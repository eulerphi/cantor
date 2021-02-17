module Box exposing (Box, Boxlike, addPos, hasSize, scale, scalePos, scaleSize)

import Pos exposing (Pos)
import Size exposing (Size)


type alias Box =
    { pos : Pos
    , size : Size
    }


type alias Boxlike r =
    { r
        | pos : Pos
        , size : Size
    }


addPos : Pos -> Boxlike r -> Boxlike r
addPos pos box =
    { box | pos = Pos.add pos box.pos }


hasSize : Boxlike r -> Bool
hasSize box =
    box.size.width > 0 && box.size.height > 0


scale : Float -> Boxlike r -> Boxlike r
scale value box =
    box |> scalePos value |> scaleSize value


scalePos : Float -> Boxlike r -> Boxlike r
scalePos value box =
    { box | pos = box.pos |> Pos.scale value }


scaleSize : Float -> Boxlike r -> Boxlike r
scaleSize value box =
    { box | size = box.size |> Size.scale value }
