module Block.Internal.Component.Body exposing (..)

import Block.Internal.Section exposing (Section)
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel, ViewModel2)
import Delta exposing (Delta)
import DragState exposing (DragState)
import Grid
import List
import Pos
import Size
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx



-- VIEW


view : List (Attribute msg) -> ViewModel2 -> Svg msg
view attrs vm =
    Svg.g
        (SvgAttrs.class "block-body" :: attrs)
        (vm.sections |> List.map (viewRect vm))


viewRect : ViewModel2 -> Section -> Svg msg
viewRect vm s =
    let
        grid =
            Grid.view [] (Grid.forBox vm.grid.unit s)

        txt =
            viewTxt vm s
    in
    Svg.g
        [ SvgAttrs.class s.class ]
        [ grid, txt ]


viewTxt : ViewModel2 -> Section -> Svg msg
viewTxt vm s =
    SvgEx.centeredText
        [ SvgAttrs.class (s.class ++ "-text") ]
        s.pos
        (Size.fromInt ( vm.grid.unit, vm.grid.unit ))
        (String.fromInt s.quantity)



-- UPDATE


dragStart : ViewModel -> Block -> DragState Block
dragStart vm bd =
    DragState.init
        { start = vm.block.pos
        , data = bd
        , addFn = Delta.add
        }


dragStart2 : ViewModel2 -> DragBodyState
dragStart2 vm =
    DragState.init22 vm.pos


dragUpdate : Delta -> DragBodyState -> DragBodyState
dragUpdate delta data =
    data |> DragState.update delta Delta.add


dragMove2 : Grid.Data -> Block -> DragBodyState -> Block
dragMove2 _ bd data =
    { bd | pos = data.current }


dragMove : DragState Block -> Grid.Data -> Block -> Block
dragMove drag _ bd =
    { bd | pos = drag.pos.current }


dragEnd : DragState Block -> Grid.Data -> Block -> Maybe Block
dragEnd drag gd bd =
    let
        pos_ =
            Pos.roundNear
                { pos = Pos.fromInt ( gd.x, gd.y )
                , unit = toFloat gd.unit
                }
                drag.pos.current
    in
    Just { bd | pos = pos_ }


dragEnd2 : Grid.Data -> Block -> DragBodyState -> Maybe Block
dragEnd2 gd bd state =
    let
        pos_ =
            state.current
                |> Pos.roundNear
                    { pos = Pos.fromInt ( gd.x, gd.y )
                    , unit = toFloat gd.unit
                    }
    in
    Just { bd | pos = pos_ }
