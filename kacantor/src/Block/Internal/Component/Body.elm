module Block.Internal.Component.Body exposing (..)

import Block.Internal.Section as Section exposing (Section)
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel, ViewModel2)
import Box exposing (Box)
import Delta exposing (Delta)
import DragState
import Grid
import List
import Maybe.Extra
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx



-- VIEW


view : List (Attribute msg) -> ViewModel -> Svg msg
view attrs vm =
    Svg.g
        (SvgAttrs.class "block-body" :: attrs)
        (vm.sections |> List.map (viewRect vm))


view2 : List (Attribute msg) -> ViewModel2 -> Svg msg
view2 attrs vm =
    let
        elems =
            vm.sections
                |> Section.toList
                |> List.map (Section.view [])
    in
    Svg.g (SvgAttrs.class "block-body" :: attrs) elems


viewRect : ViewModel -> Section -> Svg msg
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


viewTxt : ViewModel -> Section -> Svg msg
viewTxt vm s =
    SvgEx.text_
        [ SvgAttrs.class (s.class ++ "-text") ]
        (Box s.pos (Size vm.grid.unit vm.grid.unit))
        (String.fromInt s.quantity)



-- UPDATE


dragStart : ViewModel2 -> Maybe DragBodyState
dragStart vm =
    DragState.forStart vm.pos |> Just


dragUpdate : Delta -> DragBodyState -> DragBodyState
dragUpdate delta state =
    state |> DragState.update Delta.add delta


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
