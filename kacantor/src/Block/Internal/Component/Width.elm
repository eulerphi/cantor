module Block.Internal.Component.Width exposing (..)

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



-- VIEW


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging Component.Width _ ->
            Just (viewControl attrs vm)

        Selected ->
            Just (viewControl attrs vm)

        _ ->
            Nothing


viewControl : List (Attribute msg) -> ViewModel -> Svg msg
viewControl attrs vm =
    let
        { radius, rootpos, cpos, guideVisible } =
            case vm.block.state of
                Dragging Component.Width drag ->
                    { radius = round (vm.grid.unit / 1.2)
                    , rootpos = drag.start |> Pos.addDelta drag.delta.current
                    , cpos = drag.start2 |> Pos.addDelta drag.delta.total
                    , guideVisible = True
                    }

                _ ->
                    let
                        root =
                            rootPos vm
                    in
                    { radius = round (vm.grid.unit / 1.5)
                    , rootpos = root
                    , cpos = root |> circlePos vm
                    , guideVisible = False
                    }

        vbarP1 =
            rootpos

        vbarP2 =
            vbarP1 |> Pos.addDelta (Delta.init ( 0, vm.block.size.height ))

        hbarP1 =
            vbarP1 |> Pos.addDelta (Delta.init ( 0, vm.block.size.height / 2 ))

        hbarP2 =
            Pos.init ( cpos.x, hbarP1.y )

        ( guideP1, guideP2 ) =
            if guideVisible then
                ( Pos cpos.x vm.grid.pos.y
                , Pos cpos.x (vm.grid.pos.y + vm.grid.size.height)
                )

            else
                ( Pos 0 0, Pos 0 0 )
    in
    Svg.g
        (SvgAttrs.class "width-control" :: attrs)
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
            [ SvgAttrs.x1 <| Pos.toXString vbarP1
            , SvgAttrs.y1 <| Pos.toYString vbarP1
            , SvgAttrs.x2 <| Pos.toXString vbarP2
            , SvgAttrs.y2 <| Pos.toYString vbarP2
            , SvgAttrs.strokeWidth <| String.fromFloat <| barWidth
            ]
            []
        , Svg.line
            [ SvgAttrs.x1 <| Pos.toXString hbarP1
            , SvgAttrs.y1 <| Pos.toYString hbarP1
            , SvgAttrs.x2 <| Pos.toXString hbarP2
            , SvgAttrs.y2 <| Pos.toYString hbarP2
            , SvgAttrs.strokeWidth <| String.fromFloat <| barWidth
            ]
            []
        , Svg.circle
            [ SvgAttrs.cx <| Pos.toXString cpos
            , SvgAttrs.cy <| Pos.toYString cpos
            , SvgAttrs.r <| String.fromInt <| radius
            ]
            []
        ]


barOffset : Float
barOffset =
    4


barWidth : Float
barWidth =
    3


circlePos : ViewModel -> Pos -> Pos
circlePos vm root =
    root |> Pos.addDelta (Delta vm.grid.unit (vm.block.size.height / 2))


rootPos : ViewModel -> Pos
rootPos vm =
    let
        rootElement =
            Maybe.withDefault vm.body.mid vm.body.top

        delta =
            Delta (rootElement.size.width + barOffset) 0
    in
    rootElement.pos |> Pos.addDelta delta



-- UPDATE


startDrag : ViewModel -> Block -> DragState Block
startDrag vm bd =
    let
        rootpos =
            rootPos vm
    in
    DragState.init2
        { start = rootpos
        , start2 = rootpos |> circlePos vm
        , data = bd
        , addFn = Delta.addX
        }


dragMove : DragState Block -> Grid.Data -> Block -> Block
dragMove drag gd bd =
    let
        unitDelta =
            drag.delta.current
                |> Delta.roundNear (toFloat gd.unit)
                |> Delta.div (toFloat gd.unit)

        dx =
            round unitDelta.dx

        width_ =
            max 1 (drag.data.width + dx)

        headerOffset_ =
            min
                bd.headerOffset
                (width_ - 1)
    in
    { bd | headerOffset = headerOffset_, width = width_ }


dragEnd : DragState Block -> Grid.Data -> Block -> Maybe Block
dragEnd _ _ bd =
    Just bd
