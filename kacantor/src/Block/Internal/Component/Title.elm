module Block.Internal.Component.Title exposing (..)

import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel)
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging _ (DragOffset _) ->
            Just <| viewTitle vm

        Dragging _ (DragQuantity _) ->
            Just <| viewTitle vm

        Dragging _ (DragWidth _) ->
            Just <| viewTitle vm

        Selected ->
            Just <| viewTitle vm

        _ ->
            Nothing


viewTitle : ViewModel -> Svg msg
viewTitle vm =
    Svg.g [] []



-- let
--     txt =
--         vm.body.str1
--     size =
--         titleSize vm txt
--     pos =
--         rootPosition vm size
--     ( lineP1, lineP2 ) =
--         ( Pos (pos.x + size.width) pos.y
--         , Pos (vm.block.pos.x - 2) (vm.block.pos.y + vm.block.size.height + 2)
--         )
-- in
-- Svg.g
--     [ SvgAttrs.class "title" ]
--     [ Svg.line
--         [ SvgAttrs.x1 <| Pos.toXString lineP1
--         , SvgAttrs.y1 <| Pos.toYString lineP1
--         , SvgAttrs.x2 <| Pos.toXString lineP2
--         , SvgAttrs.y2 <| Pos.toYString lineP2
--         ]
--         []
--     , Svg.rect
--         [ SvgAttrs.x <| Pos.toXString pos
--         , SvgAttrs.y <| Pos.toYString pos
--         , SvgAttrs.width <| Size.toWidthString size
--         , SvgAttrs.height <| Size.toHeightString size
--         ]
--         []
--     , SvgEx.centeredText
--         []
--         pos
--         size
--         txt
--     ]


titleSize : ViewModel -> String -> Size
titleSize vm _ =
    Size (3 * vm.grid.unit / 2) (3 * vm.grid.unit / 2)


rootPosition : ViewModel -> Size -> Pos
rootPosition vm size =
    vm.block.pos
        |> Pos.addX -(size.width + vm.grid.unit / 2)
        |> Pos.addY (vm.size.height + vm.grid.unit / 2)
