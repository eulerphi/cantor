module Block.Internal.Component.Title exposing (..)

import Block.Internal.Config as Config
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel)
import Box exposing (Box)
import Line
import Pair
import Pos
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view _ vm =
    case vm.block.state of
        Dragging _ _ ->
            viewTitle vm |> Just

        Selected ->
            Just <| viewTitle vm

        _ ->
            Nothing


viewTitle : ViewModel -> Svg msg
viewTitle vm =
    let
        line =
            vm.pos
                |> Pos.addX -Config.outlinePadding
                |> Pos.addY (vm.size.height + Config.outlinePadding)
                |> Line.addXY -(vm.grid.unit / 2 - Config.outlinePadding) (vm.grid.unit / 2)

        txt =
            vm.block.quantity |> String.fromInt

        txtBox =
            (3 * vm.grid.unit / 2)
                |> Pair.fork
                    (\x -> line.p2 |> Pos.addX -x)
                    Size.forSquare
                |> Pair.uncurry Box
    in
    Svg.g
        [ SvgAttrs.class "title" ]
        [ SvgEx.line [] line
        , SvgEx.textWithBackground [] txtBox txt
        ]


titleSize : ViewModel -> String -> Size
titleSize vm _ =
    Size (3 * vm.grid.unit / 2) (3 * vm.grid.unit / 2)
