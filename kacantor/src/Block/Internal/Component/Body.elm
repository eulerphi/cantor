module Block.Internal.Component.Body exposing (..)

import Block.Internal.Section exposing (Section)
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel, ViewModel2)
import Delta exposing (Delta)
import DragState exposing (DragState)
import Grid
import List
import Pos exposing (Pos)
import Size exposing (Size)
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
        (Size vm.grid.unit vm.grid.unit)
        (String.fromInt s.quantity)



-- UPDATE


dragStart : ViewModel2 -> Maybe DragBodyState
dragStart vm =
    DragState.init22 vm.pos |> Just


dragUpdate : Delta -> DragBodyState -> DragBodyState
dragUpdate delta state =
    state |> DragState.update delta Delta.add


dragMove : DragContext -> DragBodyState -> Block
dragMove { bd } { current } =
    current |> updatePos bd


dragEnd : DragContext -> DragBodyState -> Maybe Block
dragEnd { gd, bd } { current } =
    current
        |> Pos.roundNear gd
        |> updatePos bd
        |> Just


updatePos : Block -> Pos -> Block
updatePos bd pos =
    { bd | pos = pos }
