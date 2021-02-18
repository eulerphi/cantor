module DragState exposing (..)

import Delta exposing (Delta)
import Pos exposing (Pos)


type alias DragState a =
    { addFn : AddFunction
    , data : a
    , delta : DragDelta
    , pos : DragPos

    -- total hack :)
    , start : Pos
    , start2 : Pos
    , start3 : Pos
    }


type alias DragState2 =
    { current : Pos
    , delta : Delta
    , start : Pos
    }


forStart : Pos -> DragState2
forStart start =
    DragState2 start Delta.none start


update : Delta -> AddFunction -> DragState2 -> DragState2
update delta addFn state =
    let
        delta_ =
            delta |> addFn state.delta

        current_ =
            state.start |> Pos.addDelta delta_
    in
    { state | current = current_, delta = delta_ }


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
    , start2 = Pos.origin
    , start3 = Pos.origin
    }


init2 :
    { start : Pos
    , start2 : Pos
    , data : a
    , addFn : AddFunction
    }
    -> DragState a
init2 input =
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
    , start2 = input.start2
    , start3 = Pos.origin
    }


init3 :
    { start : Pos
    , start2 : Pos
    , start3 : Pos
    , data : a
    , addFn : AddFunction
    }
    -> DragState a
init3 input =
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
    , start2 = input.start2
    , start3 = input.start3
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
