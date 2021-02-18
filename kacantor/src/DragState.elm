module DragState exposing (..)

import Delta exposing (Delta)
import Pos exposing (Pos)


type alias AddDeltaFunction =
    Delta -> Delta -> Delta


type alias DragState =
    { current : Pos
    , delta : Delta
    , start : Pos
    }


forStart : Pos -> DragState
forStart start =
    DragState start Delta.none start


update : AddDeltaFunction -> Delta -> DragState -> DragState
update addFn delta state =
    let
        delta_ =
            delta |> addFn state.delta

        current_ =
            state.start |> Pos.addDelta delta_
    in
    { state | current = current_, delta = delta_ }
