module Block.Internal.Component.Quantity exposing (..)

import Block.Internal.Component as Component
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel)
import Delta
import DragState exposing (DragState)
import Grid
import MathEx
import Pos exposing (Pos)
import Size
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging Component.Quantity _ ->
            Just <| viewControl attrs vm

        Selected ->
            Just <| viewControl attrs vm

        _ ->
            Nothing


viewControl : List (Attribute msg) -> ViewModel -> Svg msg
viewControl attrs vm =
    let
        ( radius, pos ) =
            case vm.block.state of
                Dragging Component.Quantity drag ->
                    ( round (vm.grid.unit / 1.2)
                    , drag.pos.total
                    )

                _ ->
                    ( round (vm.grid.unit / 1.5)
                    , rootPos vm
                    )

        lineWidth =
            3

        unit =
            vm.grid.unit

        halfUnit =
            unit / 2

        rectPos =
            let
                delta =
                    Delta.init ( -lineWidth, -lineWidth )
            in
            Pos.addDelta delta pos

        rectSize =
            Size.init ( unit + 2 * lineWidth, unit + 2 * lineWidth )

        vlineP1 =
            Pos.add rectPos <| Pos.init ( halfUnit + lineWidth, rectSize.height )

        vlineP2 =
            Pos.addY vlineP1 <| Pos.init ( 0, unit )

        -- p2 =
        --     Pos.addY p1 { x = 0.0, y = vm.block.size.height }
        circlePos =
            vlineP2
    in
    Svg.g
        (SvgAttrs.class "quantity-control" :: attrs)
        [ Svg.rect
            [ SvgAttrs.x <| Pos.toXString rectPos
            , SvgAttrs.y <| Pos.toYString rectPos
            , SvgAttrs.width <| Size.toWidthString rectSize
            , SvgAttrs.height <| Size.toHeightString rectSize
            , SvgAttrs.height <| Size.toHeightString rectSize
            , SvgAttrs.fillOpacity "0"
            ]
            []
        , Svg.line
            [ SvgAttrs.x1 <| Pos.toXString vlineP1
            , SvgAttrs.y1 <| Pos.toYString vlineP1
            , SvgAttrs.x2 <| Pos.toXString vlineP2
            , SvgAttrs.y2 <| Pos.toYString vlineP2
            , SvgAttrs.strokeWidth <| String.fromFloat <| lineWidth
            ]
            []
        , Svg.circle
            [ SvgAttrs.cx <| Pos.toXString circlePos
            , SvgAttrs.cy <| Pos.toYString circlePos
            , SvgAttrs.r <| String.fromInt <| radius
            ]
            []
        ]


rootPos : ViewModel -> Pos
rootPos vm =
    let
        root =
            Maybe.withDefault vm.body.mid vm.body.bot

        delta =
            Delta.init
                ( root.size.width - vm.grid.unit
                , root.size.height - vm.grid.unit
                )
    in
    Pos.addDelta delta root.pos



-- UPDATE


startDrag : ViewModel -> Block -> DragState Block
startDrag vm bd =
    DragState.init
        { start = rootPos vm
        , data = bd
        , addFn = Delta.add
        }


dragMove : DragState Block -> Grid.Data -> Block -> Block
dragMove drag gd bd =
    let
        unitDelta =
            drag.delta.current
                |> Delta.roundNear (toFloat gd.unit)
                |> Delta.div (toFloat gd.unit)

        -- TODO: handle offset remainder
        remainder =
            modBy drag.data.width drag.data.quantity

        minY =
            if remainder > 0 then
                remainder

            else
                drag.data.width

        dy =
            round unitDelta.dy * drag.data.width

        quantity_ =
            max minY (drag.data.quantity + dy)

        ( minX, maxX ) =
            if remainder > 0 then
                ( -remainder, drag.data.width - remainder )

            else
                ( -drag.data.width, 0 )

        dx =
            MathEx.minmax minX maxX (round unitDelta.dx)

        quantity__ =
            max 0 (quantity_ + dx)
    in
    { bd | quantity = quantity__ }


dragEnd : DragState Block -> Grid.Data -> Block -> Maybe Block
dragEnd _ _ bd =
    if bd.quantity <= 0 then
        Nothing

    else
        Just bd
