module Block.Internal.Component.Quantity exposing (..)

import Block.Internal.Component exposing (Component(..))
import Block.Internal.Section as Section
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel)
import Box exposing (Box)
import CircleDragControl as CircleControl
import Delta exposing (Delta)
import DragState
import Grid exposing (Grid)
import Line
import Maybe.Extra
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging _ (DragQuantity state) ->
            { active = True, pos = state.current }
                |> viewControl attrs vm
                |> Just

        Selected ->
            vm
                |> rootPosition
                |> Maybe.map (\pos -> { active = False, pos = pos })
                |> Maybe.map (viewControl attrs vm)

        _ ->
            Nothing


viewControl :
    List (Attribute msg)
    -> ViewModel
    -> { active : Bool, pos : Pos }
    -> Svg msg
viewControl attrs vm { active, pos } =
    let
        ( unit, halfUnit ) =
            ( vm.grid.unit, vm.grid.unit / 2 )

        rect =
            Box pos (Size unit unit)

        vline =
            rect.pos
                |> Pos.add (Pos halfUnit rect.size.height)
                |> Line.addY (2 * unit)

        cpos =
            vline.p2
    in
    Svg.g
        [ SvgAttrs.class "quantity-control" ]
        [ SvgEx.rect [] rect
        , SvgEx.line [] vline
        , CircleControl.view
            attrs
            { active = active
            , pos = cpos
            , unit = vm.grid.unit
            }
        ]


rootPosition : ViewModel -> Maybe Pos
rootPosition vm =
    vm.sections
        |> Section.last
        |> Maybe.map
            (\s ->
                s.pos
                    |> Pos.addX (s.size.width - vm.grid.unit)
                    |> Pos.addY (s.size.height - vm.grid.unit)
            )



-- UPDATE


dragStart : ViewModel -> Maybe DragQuantityState
dragStart vm =
    vm |> rootPosition |> Maybe.map DragState.forStart


dragUpdate : Delta -> DragQuantityState -> DragQuantityState
dragUpdate delta data =
    data |> DragState.update Delta.add delta


dragMove : DragContext -> DragQuantityState -> Block
dragMove { gd, bd } { current } =
    current
        |> calculateQuantity gd bd
        |> updateQuantity bd


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

        ( dx, dy ) =
            pos_
                |> Pos.deltaBetween bd.pos
                |> Delta.div gd.unit
                |> Delta.map round
                |> Tuple.mapBoth (\x -> max x -1) (\y -> max y 0)

        quantity_ =
            (dy * bd.width) + min (dx + 1) bd.width - bd.headerOffset
    in
    max 0 quantity_


updateQuantity : Block -> Int -> Block
updateQuantity bd quantity_ =
    { bd | quantity = quantity_ }
