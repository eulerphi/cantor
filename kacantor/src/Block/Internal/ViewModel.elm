module Block.Internal.ViewModel exposing (..)

import Block.Internal.Body as Body exposing (Body)
import Block.Model as Model
import Grid
import Pos exposing (Pos)
import Size exposing (Size)


type alias ViewModel =
    { body : Body
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
                Model.Dragging Model.Body ( dx, dy ) ->
                    ( dx, dy )

                _ ->
                    ( 0, 0 )

        pos =
            Pos.fromInt ( bd.x, bd.y )
                |> Pos.scaleByInt gd.unit
                |> Pos.add3 (Pos.fromInt ( gd.x, gd.y )) (Pos.fromInt delta)

        body =
            Body.toBody gd bd
    in
    { body = body
    , block =
        { pos = pos
        , size = Body.size body
        , state = bd.state
        , width = bd.width
        }
    , grid =
        { pos = Pos.fromInt ( gd.x, gd.y )
        , size = Size.fromInt ( gd.width, gd.height )
        , unit = gd.unit
        }
    }
