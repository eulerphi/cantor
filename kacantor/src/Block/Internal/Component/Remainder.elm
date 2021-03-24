module Block.Internal.Component.Remainder exposing (..)

import Block.Internal.Component exposing (Component(..))
import Block.Internal.Config as Config
import Block.Internal.Section as Section
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel, ViewModel2)
import Box exposing (Box)
import CircleDragControl as CircleControl
import Delta exposing (Delta)
import DragState
import Grid exposing (Grid)
import Line
import MathEx
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
        Dragging _ (RemainderDrag state) ->
            { active = True, pos = state.current }
                |> viewControl attrs vm
                |> Just

        Selected ->
            vm
                |> rootPosition2
                |> Maybe.map (\pos -> { active = False, pos = pos })
                |> Maybe.map (viewControl attrs vm)

        _ ->
            Nothing


viewControl :
    List (Attribute msg)
    -> ViewModel2
    -> { active : Bool, pos : Pos }
    -> Svg msg
viewControl attrs vm { active, pos } =
    let
        ( unit, halfUnit ) =
            ( vm.grid.unit, vm.grid.unit / 2 )

        vline =
            pos |> Line.addY unit

        hline =
            pos
                |> Pos.addY halfUnit
                |> Line.addX unit

        cpos =
            vline.p2
    in
    Svg.g
        [ SvgAttrs.class "remainder-control" ]
        [ SvgEx.line [] vline
        , CircleControl.view3
            attrs
            { active = active
            , offsetAnchor = OffsetAnchor HCenter Bottom
            , pos = cpos
            , unit = vm.grid.unit
            , txt = ""
            }
        ]


rootPosition : ViewModel -> Maybe Pos
rootPosition vm =
    vm.sections
        |> Section.remainderPos (OffsetAnchor Right Bottom)
        |> Pos.addY -vm.grid.unit
        |> Pos.addX Config.outlinePadding
        |> Just


rootPosition2 : ViewModel2 -> Maybe Pos
rootPosition2 vm =
    vm.sections.remainder
        |> Maybe.Extra.orElse vm.sections.product
        |> Maybe.map (OffsetAnchor.toPos (OffsetAnchor Right Bottom))
        |> Maybe.map (Pos.addY -vm.grid.unit)
        |> Maybe.map (Pos.addX Config.outlinePadding)



-- UPDATE


dragStart : ViewModel -> Maybe DragQuantityState
dragStart vm =
    vm |> rootPosition |> Maybe.map DragState.forStart


dragUpdate : Delta -> DragQuantityState -> DragQuantityState
dragUpdate delta data =
    data |> DragState.update Delta.add delta


dragMove : DragContext -> DragQuantityState -> Block
dragMove { gd, bd } { delta } =
    let
        dx =
            delta
                |> Delta.roundNear gd.unit
                |> Delta.div gd.unit
                |> .dx
                |> round

        ( q, r ) =
            let
                remainder =
                    modBy bd.width bd.quantity
            in
            if remainder == 0 then
                ( bd.quantity - bd.width, bd.width )

            else
                ( bd.quantity - remainder, remainder )

        remainder_ =
            r + dx |> MathEx.minmax 1 bd.width

        quantity_ =
            q + remainder_
    in
    { bd | quantity = quantity_ }


dragEnd : DragContext -> DragQuantityState -> Maybe Block
dragEnd ctx state =
    dragMove ctx state
        |> Just
        |> Maybe.Extra.filter (\bd -> bd.quantity > 0)


calculateQuantity : Grid -> Block -> Pos -> Int
calculateQuantity gd bd pos =
    let
        pos_ =
            pos |> Pos.roundNear gd

        ( dx, _ ) =
            pos_
                |> Pos.deltaBetween bd.pos
                |> Delta.div gd.unit
                |> Delta.map round
                |> Tuple.mapBoth (\x -> max x -1) (\y -> max y 0)

        ( remainder, remainder_ ) =
            ( modBy bd.width bd.quantity
            , min (dx + 1) bd.width
            )

        product =
            if remainder == 0 then
                bd.quantity - bd.width

            else
                bd.quantity - remainder

        quantity_ =
            product + remainder_
    in
    max 0 quantity_


updateQuantity : Block -> Int -> Block
updateQuantity bd quantity_ =
    { bd | quantity = quantity_ }
