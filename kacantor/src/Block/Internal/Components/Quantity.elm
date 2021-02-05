module Block.Internal.Components.Quantity exposing (..)

import Block.Internal.Component as Component exposing (Component)
import Block.Internal.Types exposing (..)
import Block.Internal.View.BodyModel
import Block.Internal.View.Model as ViewModel exposing (ViewModel)
import Delta exposing (Delta)
import DragState exposing (DragState)
import Grid
import MathEx
import Pair
import Pos
import Size
import Svg
import Svg.Attributes as SvgAttrs


dragMove : Grid.Data -> Block -> Block
dragMove gd bd =
    case bd.state of
        Dragging Component.Quantity dragState ->
            bd

        _ ->
            bd



-- let
--     remainder =
--         modBy bd.width bd.quantity
--     quantityDeltaX =
--         dx // gd.unit
--     quantityDeltaY =
--         dy // gd.unit
--     qdx =
--         max remainder (dx // gd.unit)
--     qdy =
--         if quantityDeltaY /= 0 then
--             bd.width * quantityDeltaY
--         else
--             0
--     quantityDelta =
--         if abs quantityDeltaX > abs qdy then
--             quantityDeltaX
--         else
--             qdy
--     -- max quantityDeltaX qdy
--     quantity_ =
--         max 0 (bd.quantity + quantityDelta)
--     dx_ =
--         dx - (quantityDeltaX * gd.unit)
--     dy_ =
--         dy - (quantityDeltaY * gd.unit)
-- in
-- { bd
--     | state = Dragging AddControl ( dx_, dy_ )
--     , quantity = quantity_
-- }


view : List (Svg.Attribute msg) -> ViewModel -> Maybe (Svg.Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging Component.Quantity _ ->
            viewControl attrs vm

        Selected ->
            viewControl attrs vm

        _ ->
            Nothing


viewControl : List (Svg.Attribute msg) -> ViewModel -> Maybe (Svg.Svg msg)
viewControl attrs vm =
    Nothing



-- let
--     ( radius, delta ) =
--         case vm.block.state of
--             Dragging QuantityControl ( dx, dy ) ->
--                 ( round (toFloat vm.grid.unit / 1.2)
--                 , Pos.fromInt ( dx, dy )
--                 )
--             _ ->
--                 ( round (toFloat vm.grid.unit / 1.5), Pos.origin )
--     lineWidth =
--         3
--     unit =
--         toFloat vm.grid.unit
--     halfUnit =
--         unit / 2
--     root =
--         Maybe.withDefault vm.body.mid vm.body.bot
--     rectPos =
--         let
--             offset =
--                 Pos.init ( root.size.width - unit - lineWidth, root.size.height - unit - lineWidth )
--         in
--         Pos.add4 root.pos vm.block.pos offset delta
--     rectSize =
--         Size.init ( unit + 2 * lineWidth, unit + 2 * lineWidth )
--     vlineP1 =
--         Pos.add rectPos <| Pos.init ( halfUnit + lineWidth, rectSize.height )
--     vlineP2 =
--         Pos.addY vlineP1 <| Pos.init ( 0, unit )
--     -- p2 =
--     --     Pos.addY p1 { x = 0.0, y = vm.block.size.height }
--     circlePos =
--         vlineP2
-- in
-- Svg.g
--     (SvgAttrs.class "quantity-control" :: attrs)
--     [ Svg.rect
--         [ SvgAttrs.x <| Pos.toXString rectPos
--         , SvgAttrs.y <| Pos.toYString rectPos
--         , SvgAttrs.width <| Size.toWidthString rectSize
--         , SvgAttrs.height <| Size.toHeightString rectSize
--         , SvgAttrs.height <| Size.toHeightString rectSize
--         , SvgAttrs.fillOpacity "0"
--         , SvgAttrs.stroke "black"
--         ]
--         []
--     , Svg.line
--         [ SvgAttrs.x1 <| Pos.toXString vlineP1
--         , SvgAttrs.y1 <| Pos.toYString vlineP1
--         , SvgAttrs.x2 <| Pos.toXString vlineP2
--         , SvgAttrs.y2 <| Pos.toYString vlineP2
--         , SvgAttrs.strokeWidth <| String.fromFloat <| lineWidth
--         , SvgAttrs.stroke "black"
--         ]
--         []
--     , Svg.circle
--         [ SvgAttrs.cx <| Pos.toXString circlePos
--         , SvgAttrs.cy <| Pos.toYString circlePos
--         , SvgAttrs.r <| String.fromInt <| radius
--         ]
--         []
--     ]
-- [ Svg.line
--     [ SvgAttrs.x1 <| Pos.toXString p1
--     , SvgAttrs.y1 <| Pos.toYString p1
--     , SvgAttrs.x2 <| Pos.toXString p2
--     , SvgAttrs.y2 <| Pos.toYString p2
--     , SvgAttrs.strokeWidth <| String.fromFloat <| lineWidth
--     ]
--     []
-- , Svg.circle
--     [ SvgAttrs.cx <| Pos.toXString cpos
--     , SvgAttrs.cy <| Pos.toYString cpos
--     , SvgAttrs.r <| String.fromInt <| radius
--     ]
--     []
-- ]
