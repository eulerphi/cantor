module Block.Internal.Component.Offset exposing (..)

import Block.Internal.Component as Component
import Block.Internal.Config as Config
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel)
import Delta exposing (Delta)
import DragState exposing (DragState)
import Grid
import Pos exposing (Pos)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging Component.Offset _ ->
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
                Dragging Component.Offset drag ->
                    ( Config.circleActiveSize vm.grid.unit
                    , drag.pos.total
                    )

                _ ->
                    ( Config.circleIdleSize vm.grid.unit
                    , rootPos vm
                    )

        barP1 =
            pos

        barP2 =
            pos |> Pos.addDelta (Delta 0 vm.grid.unit)

        connectorP1 =
            pos |> Pos.addDelta (Delta -vm.grid.unit (vm.grid.unit / 2))

        connectorP2 =
            connectorP1 |> Pos.addDelta (Delta vm.grid.unit 0)

        circlePos =
            connectorP1
    in
    Svg.g
        (SvgAttrs.class "offset-control" :: attrs)
        [ Svg.line
            [ SvgAttrs.x1 <| Pos.toXString barP1
            , SvgAttrs.y1 <| Pos.toYString barP1
            , SvgAttrs.x2 <| Pos.toXString barP2
            , SvgAttrs.y2 <| Pos.toYString barP2
            , SvgAttrs.strokeWidth <| String.fromFloat <| Config.barLineWidth
            ]
            []
        , Svg.line
            [ SvgAttrs.x1 <| Pos.toXString connectorP1
            , SvgAttrs.y1 <| Pos.toYString connectorP1
            , SvgAttrs.x2 <| Pos.toXString connectorP2
            , SvgAttrs.y2 <| Pos.toYString connectorP2
            , SvgAttrs.strokeWidth <| String.fromFloat <| Config.connectorLineWidth
            ]
            []
        , Svg.circle
            [ SvgAttrs.cx <| Pos.toXString circlePos
            , SvgAttrs.cy <| Pos.toYString circlePos
            , SvgAttrs.r <| String.fromFloat <| radius
            ]
            []
        ]


rootPos : ViewModel -> Pos
rootPos vm =
    let
        root =
            Maybe.withDefault vm.body.mid vm.body.top
    in
    Pos.addDelta Config.offsetPosDelta root.pos



-- UPDATE


startDrag : ViewModel -> Block -> DragState Block
startDrag vm bd =
    DragState.init
        { start = rootPos vm
        , data = bd
        , addFn = Delta.addX
        }


dragMove : DragState Block -> Grid.Data -> Block -> Block
dragMove drag gd bd =
    let
        { dx } =
            drag.delta.current
                |> Delta.roundNear (toFloat gd.unit)
                |> Delta.div (toFloat gd.unit)

        maxOffset =
            bd.width - 1

        headerOffset_ =
            min maxOffset (drag.data.headerOffset + round dx)
    in
    { bd | headerOffset = headerOffset_ }


dragEnd : DragState Block -> Grid.Data -> Block -> Maybe Block
dragEnd _ _ bd =
    Just bd
