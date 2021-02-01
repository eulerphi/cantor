module Block.Internal.Update exposing (..)

import Block.Model exposing (..)
import Grid
import MathEx
import Pair


startDrag : Id -> Data -> Data
startDrag id bd =
    { bd | state = Dragging id.part ( 0, 0 ) }


endDrag : Grid.Data -> Data -> Data
endDrag gd bd =
    let
        dragDelta =
            case bd.state of
                Dragging part delta ->
                    delta

                _ ->
                    ( 0, 0 )

        ( dx, dy ) =
            dragDelta
                |> Pair.map (MathEx.roundNear gd.unit)
                |> Pair.map (\v -> v // gd.unit)
    in
    { bd | x = bd.x + dx, y = bd.y + dy, state = Selected }


onDrag : ( Int, Int ) -> Data -> Data
onDrag newDelta bd =
    let
        state_ =
            case bd.state of
                Dragging part oldDelta ->
                    Dragging part (Pair.add oldDelta newDelta)

                _ ->
                    bd.state
    in
    { bd | state = state_ }
