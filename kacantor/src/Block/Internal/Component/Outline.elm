module Block.Internal.Component.Outline exposing (..)

import Block.Internal.Config as Config
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel2)
import Box exposing (Box)
import Pos
import Size
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel2 -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        -- Dragging _ _ ->
        --     Just (viewOutline attrs vm)
        Selected ->
            Just (viewOutline attrs vm)

        _ ->
            Nothing


viewOutline : List (Attribute msg) -> ViewModel2 -> Svg msg
viewOutline attrs vm =
    Svg.g
        (SvgAttrs.class "outline" :: attrs)
        [ SvgEx.rect
            []
            (Box
                (vm.pos |> Pos.addXY -Config.outlinePadding)
                (vm.size
                    |> Size.addWidth (2 * Config.outlinePadding)
                    |> Size.addHeight (2 * Config.outlinePadding)
                )
            )
        ]
