module Block.Internal.Component.Multiplier exposing (..)

import Block.Internal.Section as Section exposing (Section, Section2)
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
        -- Dragging _ (MultiplierDrag state) ->
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
                    (circlePosition vm.sections.product)
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
            ( vm.block.product.height |> String.fromInt
            , Line rpos cpos
                |> Line.midPos
                |> Box.center (Size.square halfUnit)
            )

        elems =
            [ SvgEx.line [ SvgAttrs.class "mainline" ] (Line rpos cpos)
            , SvgEx.line [] (rpos |> Line.centeredX quarterUnit)
            , SvgEx.textWithBackground [] txtBox txt
            ]

        opts =
            if active then
                [ SvgEx.line
                    [ SvgAttrs.class "guideline" ]
                    (rpos |> Line.toY (vm.grid |> OffsetAnchor.toY OffsetAnchor.Bottom))
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
                    (cpos |> Line.centeredX quarterUnit)
                ]
    in
    Svg.g
        [ SvgAttrs.class "multiplier" ]
        (elems ++ opts)



-- rootPosition : ViewModel -> Pos


rootPosition : { r | grid : Grid, pos : Pos, size : Size } -> Pos
rootPosition vm =
    vm.pos |> Pos.addX -(vm.grid.unit / 2)



-- circlePosition : ViewModel -> Pos -> Pos


circlePosition : Maybe { r | pos : Pos, size : Size } -> Pos -> Pos
circlePosition productSection rpos =
    productSection
        |> Maybe.map (\s -> s.size.height)
        |> Maybe.withDefault 0
        |> (\y -> Pos.addY y rpos)


dragStart : ViewModel -> Maybe DragState
dragStart vm =
    vm
        |> rootPosition
        |> circlePosition (vm.sections |> Section.midSection)
        |> DragState.forStart
        |> Just


dragUpdate : Delta -> DragState -> DragState
dragUpdate delta drag =
    drag |> DragState.update Delta.addY delta


dragMove : DragContext -> DragState -> Block
dragMove { gd, bd } state =
    let
        dy =
            state.delta
                |> Delta.roundNear gd.unit
                |> Delta.div gd.unit
                |> .dy
                |> round

        remainder =
            modBy bd.width bd.quantity

        quantity_ =
            max remainder (bd.quantity + (bd.width * dy))
    in
    { bd | quantity = quantity_ }


dragEnd : DragContext -> DragState -> Maybe Block
dragEnd ctx state =
    dragMove ctx state |> Just
