module Box exposing
    ( Box
    , Boxlike
    , addPos
    , asBox
    , center
    , hasSize
    , none
    , pad
    , scale
    , scalePos
    , scaleSize
    , updatePos
    )

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


asBox : Boxlike r -> Box
asBox box =
    Box box.pos box.size


center : Size -> Pos -> Box
center size pos =
    pos
        |> Pos.addX -(size.width / 2)
        |> Pos.addY -(size.height / 2)
        |> (\p -> Box p size)


hasSize : Boxlike r -> Bool
hasSize box =
    box.size.width > 0 && box.size.height > 0


none : Box
none =
    Box Pos.origin Size.none


pad : Float -> Boxlike r -> Boxlike r
pad value box =
    { box
        | pos =
            box.pos
                |> Pos.addX value
                |> Pos.addY value
        , size =
            box.size
                |> Size.addWidth -(2 * value)
                |> Size.addHeight -(2 * value)
    }


scale : Float -> Boxlike r -> Boxlike r
scale value box =
    box |> scalePos value |> scaleSize value


scalePos : Float -> Boxlike r -> Boxlike r
scalePos value box =
    { box | pos = box.pos |> Pos.scale value }


scaleSize : Float -> Boxlike r -> Boxlike r
scaleSize value box =
    { box | size = box.size |> Size.scale value }


updatePos : Pos -> Boxlike r -> Boxlike r
updatePos pos box =
    { box | pos = pos, size = box.size }
