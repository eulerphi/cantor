module Block.Internal.Component.Title exposing (..)

import Block.Internal.Config as Config
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel)
import Box exposing (Box)
import Line
import MathEx
import Pos
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view _ vm =
    case vm.block.state of
        Dragging ctx (DragOffset _) ->
            viewTitle vm (dragOffsetText vm ctx) |> Just

        Dragging ctx (DragQuantity _) ->
            viewTitle vm (dragQuantityText vm ctx) |> Just

        Dragging ctx (DragWidth _) ->
            viewTitle vm (dragWidthText vm ctx) |> Just

        Selected ->
            viewTitle vm (quantityText vm) |> Just

        _ ->
            Nothing


viewTitle : ViewModel -> String -> Svg msg
viewTitle vm txt =
    let
        line =
            vm.pos
                |> Pos.addX -Config.outlinePadding
                |> Pos.addY (vm.size.height + Config.outlinePadding)
                |> Line.addXY -(vm.grid.unit / 2 - Config.outlinePadding) (vm.grid.unit / 2)

        txtSize =
            (3 * vm.grid.unit / 2) |> titleSize vm txt

        txtBox =
            Box (line.p2 |> Pos.addX -txtSize.width) txtSize
    in
    Svg.g
        [ SvgAttrs.class "title" ]
        [ SvgEx.line [] line
        , SvgEx.textWithBackground [] txtBox txt
        ]


titleSize : ViewModel -> String -> Float -> Size
titleSize vm txt height =
    String.length txt
        |> toFloat
        |> (*) 13
        |> MathEx.roundNear vm.grid.unit
        |> max height
        |> (\w -> Size w height)


dragOffsetText : ViewModel -> DragContext -> String
dragOffsetText vm _ =
    vm.sections
        |> List.map .quantity
        |> List.map String.fromInt
        |> List.intersperse " + "
        |> String.concat
        |> (\s -> String.append s " = ")
        |> (\s -> String.append s (quantityText vm))


dragQuantityText : ViewModel -> DragContext -> String
dragQuantityText vm { bd } =
    let
        change =
            if vm.block.quantity >= bd.quantity then
                " + " ++ (vm.block.quantity - bd.quantity |> String.fromInt)

            else
                " - " ++ (bd.quantity - vm.block.quantity |> String.fromInt)
    in
    String.fromInt bd.quantity
        ++ change
        ++ " = "
        ++ String.fromInt vm.block.quantity


dragWidthText : ViewModel -> DragContext -> String
dragWidthText vm _ =
    vm.sections
        |> List.map
            (\s ->
                if s.isMid then
                    "("
                        ++ (s.sizeInUnits.height |> String.fromInt)
                        ++ " x "
                        ++ (s.sizeInUnits.width |> String.fromInt)
                        ++ ")"

                else
                    s.quantity |> String.fromInt
            )
        |> List.intersperse " + "
        |> String.concat
        |> (\s -> String.append s " = ")
        |> (\s -> String.append s (quantityText vm))


quantityText : ViewModel -> String
quantityText vm =
    vm.block.quantity |> String.fromInt
