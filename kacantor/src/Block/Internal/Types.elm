module Block.Internal.Types exposing
    ( Block
    , Context
    , DragBodyState
    , DragComponent(..)
    , DragData
    , Id
    , Msg(..)
    , State(..)
    )

import Block.Internal.Component exposing (..)
import Box exposing (Box)
import Delta exposing (Delta)
import DragState exposing (DragState, DragState2)
import Draggable
import Grid
import Pos exposing (Pos)


type alias Block =
    { key : String
    , state : State
    , pos : Pos
    , quantity : Int
    , headerOffset : Int
    , width : Int
    }


type alias Context msg =
    { drag : Draggable.State Id
    , envelop : Msg -> msg
    }


type alias Id =
    { key : String
    , part : Component
    }


type Msg
    = DragMsg (Draggable.Msg Id)
    | StartDrag Id
    | DragMove Delta
    | EndDrag
    | Select Id


type alias DragBodyState =
    DragState2


type alias DragQuantityState =
    DragState2


type alias DragWidthState =
    { bar : DragState2
    , control : DragState2
    }


type DragComponent
    = DragBody DragBodyState
    | DragQuantity DragQuantityState
    | DragWidth DragWidthState


type alias DragData =
    { gd : Grid.Data
    , bd : Block
    , component : DragComponent
    }


type State
    = Idle
    | Dragging Component (DragState Block)
    | Dragging2 DragData
    | Selected
