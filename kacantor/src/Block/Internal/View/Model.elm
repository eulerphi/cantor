module Block.Internal.View.Model exposing (..)

import Block.Internal.Section as Section exposing (Section)
import Block.Internal.Types exposing (..)
import Block.Internal.View.BodyModel as BodyModel exposing (BodyModel)
import Box exposing (Box)
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


type alias ViewModel2 =
    { box : Box
    , block : Block
    , grid : Grid.Data
    , sections : List Section
    }


forBlock : Grid.Data -> Block -> ViewModel
forBlock gd bd =
    let
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


forBlock2 : Grid.Data -> Block -> ViewModel2
forBlock2 gd bd =
    let
        sections =
            Section.forBlock gd bd
    in
    { box = sections |> Section.toBox bd
    , block = bd
    , grid = gd
    , sections = sections
    }
