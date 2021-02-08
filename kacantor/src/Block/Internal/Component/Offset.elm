module Block.Internal.Component.Offset exposing (..)

import Block.Internal.Component as Component
import Block.Internal.Config as Config
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel)
import Delta exposing (Delta)
import DragState exposing (DragState)
import Grid
import MathEx
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
        { radius, rootpos, cpos, guideVisible } =
            case vm.block.state of
                Dragging Component.Offset drag ->
                    { radius = Config.circleActiveSize vm.grid.unit
                    , rootpos = drag.start |> Pos.addDelta drag.delta.current
                    , cpos = drag.start2 |> Pos.addDelta drag.delta.total
                    , guideVisible = True
                    }

                _ ->
                    let
                        root =
                            rootPosition vm
                    in
                    { radius = Config.circleIdleSize vm.grid.unit
                    , rootpos = root
                    , cpos = root |> circlePosition vm
                    , guideVisible = False
                    }

        barP1 =
            rootpos

        barP2 =
            rootpos |> Pos.addDelta (Delta 0 vm.grid.unit)

        connectorP1 =
            rootpos |> Pos.addDelta (Delta -vm.grid.unit (vm.grid.unit / 2))

        connectorP2 =
            connectorP1 |> Pos.addDelta (Delta vm.grid.unit 0)

        ( guideP1, guideP2 ) =
            if guideVisible then
                ( Pos cpos.x vm.grid.pos.y
                , Pos cpos.x (vm.grid.pos.y + vm.grid.size.height)
                )

            else
                ( Pos 0 0, Pos 0 0 )
    in
    Svg.g
        [ SvgAttrs.class "offset-control" ]
        [ Svg.line
            [ SvgAttrs.x1 <| Pos.toXString guideP1
            , SvgAttrs.y1 <| Pos.toYString guideP1
            , SvgAttrs.x2 <| Pos.toXString guideP2
            , SvgAttrs.y2 <| Pos.toYString guideP2
            , SvgAttrs.strokeWidth <| String.fromFloat <| Config.guideLineWidth
            , SvgAttrs.strokeDasharray "4"
            ]
            []
        , Svg.line
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
            (attrs
                ++ [ SvgAttrs.cx <| Pos.toXString cpos
                   , SvgAttrs.cy <| Pos.toYString cpos
                   , SvgAttrs.r <| String.fromFloat <| radius
                   ]
            )
            []
        ]


circlePosition : ViewModel -> Pos -> Pos
circlePosition vm rootpos =
    rootpos |> Pos.addDelta (Delta -vm.grid.unit (vm.grid.unit / 2))


rootPosition : ViewModel -> Pos
rootPosition vm =
    let
        root =
            Maybe.withDefault vm.body.mid vm.body.top
    in
    Pos.addDelta Config.offsetPosDelta root.pos



-- UPDATE


startDrag : ViewModel -> Block -> DragState Block
startDrag vm bd =
    let
        rootpos =
            rootPosition vm

        cpos =
            rootpos |> circlePosition vm
    in
    DragState.init2
        { start = rootPosition vm
        , start2 = cpos
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

        minOffset =
            0

        maxOffset =
            bd.width - 1

        headerOffset_ =
            MathEx.minmax minOffset maxOffset (drag.data.headerOffset + round dx)
    in
    { bd | headerOffset = headerOffset_ }


dragEnd : DragState Block -> Grid.Data -> Block -> Maybe Block
dragEnd _ _ bd =
    Just bd
