module Block.Internal.Component.Width exposing (..)

import Block.Internal.Component as Component
import Block.Internal.Component.Offset exposing (circlePosition)
import Block.Internal.Config as Config
import Block.Internal.Section as Section
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel, ViewModel2)
import CircleDragControl as CircleControl
import Delta exposing (Delta)
import DragState exposing (DragState, init22)
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
        { active, rootpos, cpos } =
            case vm.block.state of
                Dragging Component.Width drag ->
                    { active = True
                    , rootpos = drag.start |> Pos.addDelta drag.delta.current
                    , cpos = drag.start2 |> Pos.addDelta drag.delta.total
                    }

                _ ->
                    let
                        root =
                            rootPos vm
                    in
                    { active = False
                    , rootpos = root
                    , cpos = root |> circlePos vm
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
            if active then
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
        , CircleControl.view
            attrs
            { active = active
            , pos = cpos
            , unit = vm.grid.unit
            }
        ]


barOffset : Float
barOffset =
    4


barWidth : Float
barWidth =
    3


circlePositionXOffset : ViewModel -> Float
circlePositionXOffset vm =
    2 * vm.grid.unit


circlePos : ViewModel -> Pos -> Pos
circlePos vm root =
    root |> Pos.addDelta (Delta (circlePositionXOffset vm) (vm.block.size.height / 2))


rootPos : ViewModel -> Pos
rootPos vm =
    let
        rootElement =
            Maybe.withDefault vm.body.mid vm.body.top

        delta =
            Delta (rootElement.size.width + barOffset) 0
    in
    rootElement.pos |> Pos.addDelta delta


barPosition : ViewModel2 -> Maybe Pos
barPosition vm =
    vm.sections
        |> Section.first
        |> Maybe.map (\s -> s.pos |> Pos.addX (s.size.width + barOffset))


circlePosition : ViewModel2 -> Pos -> Pos
circlePosition vm barPos =
    barPos
        |> Pos.addX (2 * vm.grid.unit)
        |> Pos.addY (vm.size.height / 2)



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


dragStart : ViewModel2 -> Maybe DragWidthState
dragStart vm =
    vm
        |> barPosition
        |> Maybe.map
            (\pos ->
                pos
                    |> circlePosition vm
                    |> DragState.init22
                    |> DragWidthState (DragState.init22 pos)
            )


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
