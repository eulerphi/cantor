module Block.Internal.Component.Body exposing (..)

import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel)
import Delta
import DragState exposing (DragState)
import Grid
import List
import Maybe.Extra
import Pos
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx
import ViewData exposing (ViewData)



-- VIEW


view : List (Attribute msg) -> ViewModel -> Svg msg
view eventAttrs vm =
    let
        viewFn =
            viewRect vm

        optional =
            [ vm.body.top, vm.body.bot ]
                |> List.map (Maybe.map viewFn)

        elements =
            (Just (viewFn vm.body.mid) :: optional)
                |> Maybe.Extra.values
    in
    Svg.g
        (SvgAttrs.class "block-body" :: eventAttrs)
        elements


viewRect : ViewModel -> ViewData -> Svg.Svg msg
viewRect vm vd =
    let
        grid =
            Grid.forViewData (round vm.grid.unit) vd

        gridElement =
            Grid.view [] grid

        --hack time
        { width, height } =
            vd.size
                |> Size.scale (1 / vm.grid.unit)

        quantity =
            vm.block.width * (round height - 1) + round width

        txt =
            SvgEx.centeredText
                [ SvgAttrs.class (vd.class ++ "-text") ]
                vd.pos
                (Size vm.grid.unit vm.grid.unit)
                (String.fromInt quantity)
    in
    Svg.g
        [ SvgAttrs.class vd.class ]
        [ gridElement, txt ]



-- UPDATE


startDrag : ViewModel -> Block -> DragState Block
startDrag vm bd =
    DragState.init
        { start = vm.block.pos
        , data = bd
        , addFn = Delta.add
        }


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
