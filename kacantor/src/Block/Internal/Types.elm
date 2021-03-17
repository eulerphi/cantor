module Block.Internal.Types exposing
    ( Block
    , ComponentDragState(..)
    , Context
    , DragBodyState
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
import Size exposing (IntSize)


type alias Block =
    { key : String
    , state : State
    , pos : Pos
    , quantity : Int
    , headerOffset : Int
    , width : Int
    , size : IntSize
    , remainder : Int
    }


type BodyType
    = Top
    | Mid
    | Bot


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


type ComponentDragState
    = BodyDrag DragBodyState
    | MultiplicandDrag DragState
    | MultiplierDrag DragState
    | OffsetDrag DragOffsetState
    | QuantityDrag DragQuantityState
    | RemainderDrag DragState
    | WidthDrag DragWidthState


type alias DragContext =
    { gd : Grid
    , bd : Block
    }


type State
    = Idle
    | Dragging DragContext ComponentDragState
    | Selected
