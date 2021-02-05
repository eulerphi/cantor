module Block.Internal.Types exposing (Block, Context, Id, Msg(..), State(..))

import Block.Internal.Component exposing (..)
import Delta exposing (Delta)
import DragState exposing (DragState)
import Draggable
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


type State
    = Idle
    | Dragging Component (DragState Block)
    | Selected
