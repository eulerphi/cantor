module Block.Model exposing (..)

import Delta exposing (Delta)
import DragState exposing (DragState)
import Draggable
import Pos exposing (Pos)


type alias Data =
    { key : String
    , state : State
    , x : Int
    , y : Int
    , quantity : Int
    , headerOffset : Int
    , width : Int
    }


type alias Scale =
    { dx : Int
    , dy : Int
    , unit : Int
    }


type Part
    = Body
    | QuantityControl
    | OffsetControl
    | WidthControl


type State
    = Idle
    | Dragging Part DragState
    | Selected


type alias Group msg =
    { active : Maybe Data
    , context : Context msg
    , rest : List Data
    }


type Msg
    = DragMsg (Draggable.Msg Id)
    | StartDrag Id
    | DragMove Delta
    | EndDrag
    | Select Id


type alias Context msg =
    { drag : Draggable.State Id
    , envelop : Msg -> msg
    }


type alias Id =
    { key : String
    , part : Part
    }
