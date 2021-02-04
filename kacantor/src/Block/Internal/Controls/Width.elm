module Block.Internal.Controls.Width exposing (..)

import Block.Internal.ViewModel as ViewModel exposing (ViewModel)
import Block.Internal.ViewModel.Body
import Block.Model exposing (..)
import Delta exposing (Delta)
import Grid
import MathEx
import Pair
import Pos
import Svg
import Svg.Attributes as SvgAttrs



-- UPDATE


updateDragMoveDelta : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
updateDragMoveDelta oldDelta newDelta =
    Pair.add oldDelta newDelta


updateWidth : Grid.Data -> Data -> Data
updateWidth gd bd =
    case bd.state of
        Dragging WidthControl delta ->
            bd

        _ ->
            bd



-- dragMove2 : Delta -> Grid.Data -> Data -> Data
-- dragMove2 newDelta gd bd =
--     case bd.state of
--         DraggingWidthControl oldDelta ->
--             let
--                 { dx, dy } =
--                     Delta.addX oldDelta newDelta
--                 expectedWidthDelta =
--                     round dx // gd.unit
--                 width_ =
--                     MathEx.minmax 1 bd.quantity (bd.width + expectedWidthDelta)
--                 actualWidthDelta =
--                     width_ - bd.width
--                 dx_ =
--                     toFloat <| round dx - (actualWidthDelta * gd.unit)
--             in
--             { bd
--                 | state = DraggingWidthControl (Delta dx_ dy)
--                 , width = width_
--             }
--         _ ->
--             bd


dragMove : ( Int, Int ) -> Grid.Data -> Data -> Data
dragMove ( dx, dy ) gd bd =
    bd



-- let
--     -- what if state is wrong...
--     -- vm =
--     --     ViewModel.forBlock gd bd
--     deltaWidth =
--         dx // gd.unit
--     dx_ =
--         dx - (deltaWidth * gd.unit)
--     width_ =
--         max 1 (min (bd.width + deltaWidth) bd.quantity)
-- in
-- { bd
--     | state = Dragging WidthControl ( dx_, dy )
--     , width = width_
-- }
-- VIEW


view : List (Svg.Attribute msg) -> ViewModel -> Maybe (Svg.Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging WidthControl _ ->
            Just (viewControl attrs vm)

        Selected ->
            Just (viewControl attrs vm)

        _ ->
            Nothing


viewControl : List (Svg.Attribute msg) -> ViewModel -> Svg.Svg msg
viewControl attrs vm =
    Svg.g [] []



-- let
--     ( radius, delta ) =
--         case vm.block.state of
--             Dragging WidthControl ( dx, dy ) ->
--                 ( round (toFloat vm.grid.unit / 1.2)
--                 , Pos.fromInt ( dx, dy )
--                 )
--             _ ->
--                 ( round (toFloat vm.grid.unit / 1.5), Pos.origin )
--     lineWidth =
--         3.0
--     root =
--         Maybe.withDefault vm.body.mid vm.body.top
--     p1 =
--         Pos.add4
--             (Pos.init ( root.size.width + lineWidth, root.pos.y ))
--             vm.block.pos
--             root.pos
--             { x = delta.x, y = 0.0 }
--     p2 =
--         Pos.addY p1 { x = 0.0, y = vm.block.size.height }
--     cpos =
--         Pos.addY p1 delta
-- in
-- Svg.g
--     (SvgAttrs.class "width-control" :: attrs)
--     [ Svg.line
--         [ SvgAttrs.x1 <| Pos.toXString p1
--         , SvgAttrs.y1 <| Pos.toYString p1
--         , SvgAttrs.x2 <| Pos.toXString p2
--         , SvgAttrs.y2 <| Pos.toYString p2
--         , SvgAttrs.strokeWidth <| String.fromFloat <| lineWidth
--         ]
--         []
--     , Svg.circle
--         [ SvgAttrs.cx <| Pos.toXString cpos
--         , SvgAttrs.cy <| Pos.toYString cpos
--         , SvgAttrs.r <| String.fromInt <| radius
--         ]
--         []
--     ]
