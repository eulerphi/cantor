module Block.Internal.Component.Title exposing (..)

import Block.Internal.Config as Config
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel)
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : ViewModel -> Maybe (Svg msg)
view vm =
    case vm.block.state of
        Dragging _ _ ->
            Just <| viewTitle vm

        Selected ->
            Just <| viewTitle vm

        _ ->
            Nothing


viewTitle : ViewModel -> Svg msg
viewTitle vm =
    let
        txt =
            vm.body.str1 ++ " = " ++ vm.body.str2

        size =
            titleSize vm txt

        pos =
            rootPosition vm size
    in
    Svg.g
        [ SvgAttrs.class "title" ]
        [ Svg.rect
            [ SvgAttrs.x <| Pos.toXString pos
            , SvgAttrs.y <| Pos.toYString pos
            , SvgAttrs.width <| Size.toWidthString size
            , SvgAttrs.height <| Size.toHeightString size
            ]
            []
        , SvgEx.centeredText
            []
            pos
            size
            txt
        ]


titleSize : ViewModel -> String -> Size
titleSize vm txt =
    Size
        (16 / 1.5 * (toFloat <| String.length txt))
        vm.grid.unit


rootPosition : ViewModel -> Size -> Pos
rootPosition vm size =
    vm.block.pos
        |> Pos.addY -(4 * vm.grid.unit / 3)
        |> Pos.addX (vm.block.size.width / 2)
        |> Pos.addX -(size.width / 2)
