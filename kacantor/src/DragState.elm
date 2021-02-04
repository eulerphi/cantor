module DragState exposing (..)

import Delta exposing (Delta)
import Pos exposing (Pos)


type alias DragState =
    { start : Pos
    , unit : Float
    , addFn : AddFunction
    , currentUnits : Delta
    , total : Delta
    }


type alias AddFunction =
    Delta -> Delta -> Delta


init : Pos -> Float -> AddFunction -> DragState
init start unit addFn =
    { start = start
    , unit = unit
    , addFn = addFn
    , currentUnits = Delta.none
    , total = Delta.none
    }


add : Delta -> DragState -> DragState
add delta state =
    let
        total_ =
            state.addFn state.total delta

        currentUnits_ =
            total_
                |> Delta.roundNear state.unit
                |> Delta.div state.unit
    in
    { state
        | currentUnits = currentUnits_
        , total = total_
    }
