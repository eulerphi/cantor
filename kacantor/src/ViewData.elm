module ViewData exposing (..)

import Pos exposing (Pos)
import Size exposing (Size)


type alias ViewData =
    { class : String
    , pos : Pos
    , size : Size
    }


add :
    { v | pos : Pos, size : Size }
    -> ViewData
    -> ViewData
add val vd =
    { vd
        | pos = Pos.add val.pos vd.pos
        , size = Size.add val.size vd.size
    }


addPos :
    { v | pos : Pos }
    -> ViewData
    -> ViewData
addPos val vd =
    { vd | pos = Pos.add val.pos vd.pos }


hasSize : ViewData -> Bool
hasSize vd =
    vd.size.width > 0 && vd.size.height > 0


scaleByInt : Int -> ViewData -> ViewData
scaleByInt unit vd =
    { vd
        | pos = vd.pos |> Pos.scaleByInt unit
        , size = vd.size |> Size.scaleByInt unit
    }
