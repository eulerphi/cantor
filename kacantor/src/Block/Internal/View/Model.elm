module Block.Internal.View.Model exposing (..)

import Block.Internal.Component as Component exposing (Component)
import Block.Internal.Types exposing (..)
import Block.Internal.View.BodyModel as BodyModel exposing (BodyModel)
import Delta
import Grid
import Pos exposing (Pos)
import Size exposing (Size)


type alias ViewModel =
    { body : BodyModel
    , block :
        { pos : Pos
        , size : Size
        , state : State
        , width : Int
        }
    , grid :
        { pos : Pos
        , size : Size
        , unit : Float
        }
    }


forBlock : Grid.Data -> Block -> ViewModel
forBlock gd bd =
    let
        -- delta =
        --     case bd.state of
        --         -- Dragging Component.Body dragState ->
        --         --     dragdeltas.total
        --         _ ->
        --             Delta.none
        -- pos =
        --     Pos.fromInt ( bd.x, bd.y )
        --         |> Pos.scaleByInt gd.unit
        --         |> Pos.add (Pos.fromInt ( gd.x, gd.y ))
        --         |> Pos.addDelta delta
        body =
            BodyModel.forBlock gd bd
    in
    { body = body
    , block =
        { pos = bd.pos
        , size = BodyModel.size body
        , state = bd.state
        , width = bd.width
        }
    , grid =
        { pos = Pos.fromInt ( gd.x, gd.y )
        , size = Size.fromInt ( gd.width, gd.height )
        , unit = toFloat gd.unit
        }
    }
