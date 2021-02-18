module Block.Internal.Component.Offset exposing (..)

import Block.Internal.Config as Config
import Block.Internal.Section as Section
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel2)
import CircleDragControl as CircleControl
import Delta exposing (Delta)
import DragState
import Line exposing (Line)
import MathEx
import Pair
import Pos exposing (Pos)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel2 -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging _ (DragOffset { root, control }) ->
            { active = True
            , rootPos = root.current
            , controlPos = control.current
            }
                |> viewControl attrs vm
                |> Just

        Selected ->
            vm
                |> rootPosition
                |> Maybe.map
                    (\pos ->
                        { active = False
                        , rootPos = pos
                        , controlPos = pos |> circlePosition vm
                        }
                    )
                |> Maybe.map (viewControl attrs vm)

        _ ->
            Nothing


viewControl :
    List (Attribute msg)
    -> ViewModel2
    -> { active : Bool, rootPos : Pos, controlPos : Pos }
    -> Svg msg
viewControl attrs vm { active, rootPos, controlPos } =
    let
        vline =
            rootPos |> Line.addY vm.grid.unit

        hline =
            rootPos
                |> Pos.addY (vm.grid.unit / 2)
                |> Pair.fork
                    identity
                    (Pos.updateX controlPos.x)
                |> Pair.uncurry Line

        guidelines =
            if active then
                [ controlPos |> SvgEx.verticalGuideline [] vm.grid ]

            else
                []
    in
    Svg.g
        (SvgAttrs.class "offset-control" :: attrs)
        (guidelines
            ++ [ SvgEx.line [] vline
               , SvgEx.line [] hline
               , CircleControl.view
                    attrs
                    { active = active
                    , pos = controlPos
                    , unit = vm.grid.unit
                    }
               ]
        )


circlePosition : ViewModel2 -> Pos -> Pos
circlePosition vm rootpos =
    rootpos
        |> Pos.addX -(2 * vm.grid.unit)
        |> Pos.addY (vm.grid.unit / 2)


rootPosition : ViewModel2 -> Maybe Pos
rootPosition vm =
    vm.sections
        |> Section.first
        |> Maybe.map .pos
        |> Maybe.map (Pos.addX -Config.outlinePadding)



-- UPDATE


dragStart : ViewModel2 -> Maybe DragOffsetState
dragStart vm =
    vm
        |> rootPosition
        |> Maybe.map
            (Pair.fork
                DragState.forStart
                (DragState.forStart << circlePosition vm)
            )
        |> Maybe.map (Pair.uncurry DragOffsetState)


dragUpdate : Delta -> DragOffsetState -> DragOffsetState
dragUpdate delta { root, control } =
    DragOffsetState
        (root |> DragState.update Delta.addX delta)
        (control |> DragState.update Delta.add delta)


dragMove : DragContext -> DragOffsetState -> Block
dragMove { gd, bd } { root } =
    let
        dx =
            root.delta
                |> Delta.roundNear gd.unit
                |> Delta.div gd.unit
                |> .dx
                |> round

        ( minOffset, maxOffset ) =
            ( 0, bd.width - 1 )

        headerOffset_ =
            MathEx.minmax minOffset maxOffset (bd.headerOffset + dx)
    in
    { bd | headerOffset = headerOffset_ }


dragEnd : DragContext -> DragOffsetState -> Maybe Block
dragEnd ctx state =
    dragMove ctx state |> Just
