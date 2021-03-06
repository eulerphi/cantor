module Block.Internal.Component.Width exposing (..)

import Block.Internal.Component.Offset exposing (circlePosition)
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel, ViewModel2)
import CircleDragControl as CircleControl
import Delta exposing (Delta)
import DragState
import Grid exposing (Grid)
import Line exposing (Line)
import OffsetAnchor exposing (HorizontalAnchor(..), OffsetAnchor(..), VerticalAnchor(..))
import Pair
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx



-- VIEW


view : List (Attribute msg) -> ViewModel2 -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        -- Dragging _ (WidthDrag { root, control }) ->
        --     { active = True
        --     , rootPos = root.current
        --     , controlPos = control.current
        --     }
        --         |> viewControl attrs vm
        --         |> Just
        Selected ->
            vm
                |> rootPosition
                |> (\pos ->
                        { active = False
                        , rootPos = pos
                        , controlPos = pos |> circlePosition vm
                        }
                   )
                |> viewControl attrs vm
                |> Just

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
            rootPos |> Line.addY vm.size.height

        hline =
            vline.p1
                |> Pos.addY (vm.size.height / 2)
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
        (SvgAttrs.class "width-control" :: attrs)
        (guidelines
            ++ [ SvgEx.line [] vline
               , CircleControl.view3
                    attrs
                    { active = active
                    , offsetAnchor = OffsetAnchor HCenter Bottom
                    , pos = vline.p2
                    , unit = vm.grid.unit
                    , txt = ""
                    }
               ]
        )


vlineXOffset : Float
vlineXOffset =
    4


rootPosition : { r | pos : Pos, size : Size } -> Pos
rootPosition vm =
    vm.pos
        |> Pos.addX vm.size.width
        |> Pos.addX vlineXOffset


circlePosition : { r | grid : Grid, size : Size } -> Pos -> Pos
circlePosition vm barPos =
    barPos
        |> Pos.addX (2 * vm.grid.unit)
        |> Pos.addY (vm.size.height / 2)



-- UPDATE


dragStart : ViewModel -> Maybe DragWidthState
dragStart vm =
    vm
        |> rootPosition
        |> Pair.fork
            DragState.forStart
            (DragState.forStart << circlePosition vm)
        |> Pair.uncurry DragWidthState
        |> Just


dragUpdate : Delta -> DragWidthState -> DragWidthState
dragUpdate delta { root, control } =
    DragWidthState
        (root |> DragState.update Delta.addX delta)
        (control |> DragState.update Delta.add delta)


dragMove : DragContext -> DragWidthState -> Block
dragMove { gd, bd } { root } =
    let
        dx =
            root.delta
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


dragEnd : DragContext -> DragWidthState -> Maybe Block
dragEnd ctx state =
    dragMove ctx state |> Just
