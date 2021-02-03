module Block.Internal.QuantityControl exposing (..)

import Block.Internal.Body
import Block.Internal.ViewModel as ViewModel exposing (ViewModel)
import Block.Model exposing (..)
import Delta exposing (Delta)
import Grid
import MathEx
import Pair
import Pos
import Size
import Svg
import Svg.Attributes as SvgAttrs


dragMove : ( Int, Int ) -> Grid.Data -> Data -> Data
dragMove ( dx, dy ) gd bd =
    let
        remainder =
            modBy bd.width bd.quantity

        quantityDeltaX =
            dx // gd.unit

        quantityDeltaY =
            dy // gd.unit

        qdx =
            max remainder (dx // gd.unit)

        qdy =
            if quantityDeltaY /= 0 then
                bd.width * quantityDeltaY

            else
                0

        quantityDelta =
            if abs quantityDeltaX > abs qdy then
                quantityDeltaX

            else
                qdy

        -- max quantityDeltaX qdy
        quantity_ =
            max 0 (bd.quantity + quantityDelta)

        dx_ =
            dx - (quantityDeltaX * gd.unit)

        dy_ =
            dy - (quantityDeltaY * gd.unit)
    in
    { bd
        | state = Dragging AddControl ( dx_, dy_ )
        , quantity = quantity_
    }


view : List (Svg.Attribute msg) -> ViewModel -> Svg.Svg msg
view attrs vm =
    case vm.block.state of
        Dragging AddControl _ ->
            viewControl attrs vm

        Selected ->
            viewControl attrs vm

        _ ->
            Svg.g [] []


viewControl : List (Svg.Attribute msg) -> ViewModel -> Svg.Svg msg
viewControl attrs vm =
    let
        ( radius, delta ) =
            case vm.block.state of
                Dragging AddControl ( dx, dy ) ->
                    ( round (toFloat vm.grid.unit / 1.2)
                    , Pos.fromInt ( dx, dy )
                    )

                _ ->
                    ( round (toFloat vm.grid.unit / 1.5), Pos.origin )

        lineWidth =
            1.5

        unit =
            toFloat vm.grid.unit

        halfUnit =
            unit / 2

        root =
            Maybe.withDefault vm.body.mid vm.body.bot

        rectPos =
            let
                offset =
                    if root.size.width < vm.block.size.width then
                        Pos.init ( root.size.width, 0.0 )

                    else
                        Pos.init ( 0.0, root.size.height )
            in
            Pos.add4 root.pos vm.block.pos offset delta

        rectSize =
            Size.fromInt ( vm.grid.unit, vm.grid.unit )

        lineP1 =
            Pos.add rectPos <| Pos.init ( halfUnit, halfUnit )

        lineP2 =
            Pos.addY lineP1 <| Pos.init ( 0, 1.5 * unit )

        -- p2 =
        --     Pos.addY p1 { x = 0.0, y = vm.block.size.height }
        circlePos =
            lineP2
    in
    Svg.g
        (SvgAttrs.class "quantity-control" :: attrs)
        [ Svg.rect
            [ SvgAttrs.x <| Pos.toXString rectPos
            , SvgAttrs.y <| Pos.toYString rectPos
            , SvgAttrs.width <| Size.toWidthString rectSize
            , SvgAttrs.height <| Size.toHeightString rectSize
            ]
            []
        , Svg.line
            [ SvgAttrs.x1 <| Pos.toXString lineP1
            , SvgAttrs.y1 <| Pos.toYString lineP1
            , SvgAttrs.x2 <| Pos.toXString lineP2
            , SvgAttrs.y2 <| Pos.toYString lineP2
            , SvgAttrs.strokeWidth <| String.fromFloat <| lineWidth
            , SvgAttrs.stroke "black"
            ]
            []
        , Svg.circle
            [ SvgAttrs.cx <| Pos.toXString circlePos
            , SvgAttrs.cy <| Pos.toYString circlePos
            , SvgAttrs.r <| String.fromInt <| radius
            ]
            []
        ]



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
