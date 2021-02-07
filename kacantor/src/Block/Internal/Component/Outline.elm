module Block.Internal.Component.Outline exposing (..)

import Block.Internal.Config as Config
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel)
import Delta exposing (Delta)
import DragState exposing (DragState)
import Grid
import Pos exposing (Pos)
import Size
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging _ _ ->
            Just (viewOutline attrs vm)

        Selected ->
            Just (viewOutline attrs vm)

        _ ->
            Nothing


viewOutline : List (Attribute msg) -> ViewModel -> Svg msg
viewOutline attrs vm =
    let
        pos =
            vm.block.pos
                |> Pos.addDelta Config.outlinePosDelta

        size =
            vm.block.size
                |> Size.add Config.outlineSizeDelta
    in
    Svg.g
        (SvgAttrs.class "outline" :: attrs)
        [ Svg.rect
            [ SvgAttrs.x <| Pos.toXString pos
            , SvgAttrs.y <| Pos.toYString pos
            , SvgAttrs.width <| Size.toWidthString size
            , SvgAttrs.height <| Size.toHeightString size
            ]
            []
        ]


offset : Float
offset =
    4
