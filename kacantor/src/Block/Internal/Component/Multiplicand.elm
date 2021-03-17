module Block.Internal.Component.Multiplicand exposing (..)

import Block.Internal.Section as Section exposing (Section)
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel)
import Box exposing (Box)
import CircleDragControl as CircleControl
import Delta exposing (Delta)
import DragState exposing (DragState)
import Grid exposing (Grid)
import Line exposing (Line)
import Maybe.Extra
import OffsetAnchor exposing (OffsetAnchor)
import Pair
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging _ (MultiplicandDrag state) ->
            { active = True
            , rpos = rootPosition vm
            , cpos = state.current
            }
                |> viewControl attrs vm
                |> Just

        Selected ->
            rootPosition vm
                |> Pair.fork
                    identity
                    (circlePosition vm)
                |> (\( rpos, cpos ) -> { active = False, rpos = rpos, cpos = cpos })
                |> viewControl attrs vm
                |> Just

        _ ->
            Nothing


viewControl :
    List (Attribute msg)
    -> ViewModel
    -> { active : Bool, rpos : Pos, cpos : Pos }
    -> Svg msg
viewControl attrs vm { active, rpos, cpos } =
    let
        quarterUnit =
            vm.grid.unit / 4

        txt =
            (vm.size.width / vm.grid.unit)
                |> round
                |> String.fromInt
    in
    Svg.g
        [ SvgAttrs.class "multiplicand-control" ]
        [ SvgEx.line
            []
            (Line rpos cpos)
        , SvgEx.line
            [ SvgAttrs.class "guideline" ]
            (rpos |> Line.toX (OffsetAnchor.toX OffsetAnchor.Right vm.grid))
        , SvgEx.line
            []
            (rpos |> Line.centeredY quarterUnit)
        , CircleControl.view2
            attrs
            { active = active
            , pos = cpos
            , unit = vm.grid.unit
            , txt = txt
            }
        ]


rootPosition : ViewModel -> Pos
rootPosition vm =
    vm.pos |> Pos.addY -(vm.grid.unit / 2)


circlePosition : ViewModel -> Pos -> Pos
circlePosition vm rpos =
    rpos |> Pos.addX vm.size.width


dragStart : ViewModel -> Maybe DragState
dragStart vm =
    vm
        |> rootPosition
        |> circlePosition vm
        |> DragState.forStart
        |> Just


dragUpdate : Delta -> DragState -> DragState
dragUpdate delta drag =
    drag |> DragState.update Delta.addX delta


dragMove : DragContext -> DragState -> Block
dragMove { gd, bd } state =
    let
        dx =
            state.delta
                |> Delta.roundNear gd.unit
                |> Delta.div gd.unit
                |> .dx
                |> round

        width_ =
            max 1 (bd.width + dx)

        headerOffset_ =
            min bd.headerOffset (width_ - 1)
    in
    { bd | headerOffset = headerOffset_, width = width_ }


dragEnd : DragContext -> DragState -> Maybe Block
dragEnd ctx state =
    dragMove ctx state |> Just
