module Block.Model exposing (..)

import Draggable
import Pos


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
    | AddControl
    | OffsetControl
    | WidthControl


type State
    = Idle
    | Dragging Part ( Int, Int )
    | Selected


type alias Group msg =
    { active : Maybe Data
    , context : Context msg
    , rest : List Data
    }


type Msg
    = DragMsg (Draggable.Msg Id)
    | StartDrag Id
    | DragMove ( Int, Int )
    | DragWidthControl ( Int, Int )
    | EndDrag
    | Select Id
    | UpdateWidth


type alias Context msg =
    { drag : Draggable.State Id
    , envelop : Msg -> msg
    }


type alias Id =
    { key : String
    , part : Part
    }


type alias Delta =
    { dx : Int
    , dy : Int
    }
