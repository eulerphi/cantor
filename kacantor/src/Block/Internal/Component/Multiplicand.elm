module Block.Internal.Component.Multiplicand exposing (..)

import Block.Internal.Section as Section exposing (Section)
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel, ViewModel2)
import Box exposing (Box)
import CircleDragControl as CircleControl
import Delta exposing (Delta)
import DragState exposing (DragState)
import Grid exposing (Grid)
import Line exposing (Line)
import Maybe.Extra
import OffsetAnchor exposing (HorizontalAnchor(..), OffsetAnchor(..), VerticalAnchor(..))
import Pair
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel2 -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        -- Dragging _ (MultiplicandDrag state) ->
        --     { active = True
        --     , rpos = rootPosition vm
        --     , cpos = state.current
        --     }
        --         |> viewControl attrs vm
        --         |> Just
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
    -> ViewModel2
    -> { active : Bool, rpos : Pos, cpos : Pos }
    -> Svg msg
viewControl attrs vm { active, rpos, cpos } =
    let
        ( halfUnit, quarterUnit ) =
            ( vm.grid.unit / 2, vm.grid.unit / 4 )

        ( txt, txtBox ) =
            ( vm.block.product.width |> String.fromInt
            , Line rpos cpos
                |> Line.midPos
                |> Box.center (Size.square halfUnit)
            )

        elems =
            [ SvgEx.line [ SvgAttrs.class "mainline" ] (Line rpos cpos)
            , SvgEx.line [] (rpos |> Line.centeredY quarterUnit)
            , SvgEx.textWithBackground [] txtBox txt
            ]

        opts =
            if active then
                [ SvgEx.line
                    [ SvgAttrs.class "guideline" ]
                    (rpos |> Line.toX (vm.grid |> OffsetAnchor.toX OffsetAnchor.Right))
                , CircleControl.view3
                    attrs
                    { active = active
                    , offsetAnchor = OffsetAnchor HCenter VCenter
                    , pos = cpos
                    , unit = vm.grid.unit
                    , txt = ""
                    }
                ]

            else
                [ SvgEx.line
                    []
                    (cpos |> Line.centeredY quarterUnit)
                ]
    in
    Svg.g
        [ SvgAttrs.class "multiplicand-control" ]
        (elems ++ opts)



-- rootPosition : ViewModel -> Pos


rootPosition : { r | grid : Grid, pos : Pos, size : Size } -> Pos
rootPosition vm =
    vm.pos |> Pos.addY -(vm.grid.unit / 2)


circlePosition : { r | pos : Pos, size : Size } -> Pos -> Pos
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
