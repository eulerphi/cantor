module Block.Internal.ViewModel exposing (..)

import Block.Internal.ViewModel.Body as BodyViewModel exposing (BodyViewModel)
import Block.Model as Model
import Delta
import DragState exposing (DragState)
import Grid
import Pos exposing (Pos)
import Size exposing (Size)
import ViewData exposing (ViewData)


type alias ViewModel =
    { body : BodyViewModel
    , block :
        { pos : Pos
        , size : Size
        , state : Model.State
        , width : Int
        }
    , grid :
        { pos : Pos
        , size : Size
        , unit : Int
        }
    }


forBlock : Grid.Data -> Model.Data -> ViewModel
forBlock gd bd =
    let
        delta =
            case bd.state of
                Model.Dragging Model.Body dragState ->
                    dragState.total

                _ ->
                    Delta.none

        pos =
            Pos.fromInt ( bd.x, bd.y )
                |> Pos.scaleByInt gd.unit
                |> Pos.add (Pos.fromInt ( gd.x, gd.y ))
                |> Pos.addDelta delta

        body =
            BodyViewModel.forBlock gd bd
    in
    { body = body
    , block =
        { pos = pos
        , size = BodyViewModel.size body
        , state = bd.state
        , width = bd.width
        }
    , grid =
        { pos = Pos.fromInt ( gd.x, gd.y )
        , size = Size.fromInt ( gd.width, gd.height )
        , unit = gd.unit
        }
    }
