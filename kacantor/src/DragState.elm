module DragState exposing (..)

import Delta exposing (Delta)
import Pos exposing (Pos)


type alias DragState a =
    { addFn : AddFunction
    , data : a
    , delta : DragDelta
    , pos : DragPos
    , start : Pos
    }


type alias AddFunction =
    Delta -> Delta -> Delta


type alias DragPos =
    { current : Pos
    , total : Pos
    }


type alias DragDelta =
    { current : Delta
    , total : Delta
    }


init :
    { start : Pos
    , data : a
    , addFn : AddFunction
    }
    -> DragState a
init input =
    { addFn = input.addFn
    , data = input.data
    , delta =
        { current = Delta.none
        , total = Delta.none
        }
    , pos =
        { current = input.start
        , total = input.start
        }
    , start = input.start
    }


add : Delta -> DragState a -> DragState a
add delta state =
    let
        total_ =
            Delta.add state.delta.total delta

        current_ =
            state.addFn state.delta.current delta

        delta_ =
            { current = current_
            , total = total_
            }

        pos_ =
            { current = Pos.addDelta current_ state.start
            , total = Pos.addDelta total_ state.start
            }
    in
    { state | delta = delta_, pos = pos_ }
