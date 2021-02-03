module Block.Internal.Update exposing (..)

import Block.Internal.QuantityControl as QuantityControl
import Block.Internal.WidthControl as WidthControl
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
                Dragging Body delta ->
                    delta

                _ ->
                    ( 0, 0 )

        ( dx, dy ) =
            dragDelta
                |> Pair.map (MathEx.roundNear gd.unit)
                |> Pair.map (\v -> v // gd.unit)
    in
    { bd | x = bd.x + dx, y = bd.y + dy, state = Selected }


dragMove : ( Int, Int ) -> Grid.Data -> Data -> Data
dragMove newDelta gd bd =
    let
        block_ =
            case bd.state of
                Dragging AddControl oldDelta ->
                    QuantityControl.dragMove
                        (Pair.add oldDelta newDelta)
                        gd
                        bd

                Dragging Body oldDelta ->
                    { bd | state = Dragging Body (Pair.add oldDelta newDelta) }

                Dragging WidthControl oldDelta ->
                    WidthControl.dragMove
                        (Pair.add oldDelta newDelta)
                        gd
                        bd

                _ ->
                    bd
    in
    block_
