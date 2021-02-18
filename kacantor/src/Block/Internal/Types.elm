module Block.Internal.Types exposing
    ( Block
    , Context
    , DragBodyState
    , DragComponent(..)
    , DragContext
    , DragOffsetState
    , DragQuantityState
    , DragWidthState
    , Id
    , Msg(..)
    , State(..)
    )

import Block.Internal.Component exposing (..)
import Delta exposing (Delta)
import DragState exposing (DragState)
import Draggable
import Grid exposing (Grid)
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
    DragState


type alias DragOffsetState =
    { root : DragState
    , control : DragState
    }


type alias DragQuantityState =
    DragState


type alias DragWidthState =
    { root : DragState
    , control : DragState
    }


type DragComponent
    = DragBody DragBodyState
    | DragOffset DragOffsetState
    | DragQuantity DragQuantityState
    | DragWidth DragWidthState


type alias DragContext =
    { gd : Grid
    , bd : Block
    }


type State
    = Idle
    | Dragging DragContext DragComponent
    | Selected
