module Block.Internal.Component.Width exposing (..)

import Block.Internal.Component as Component exposing (Component)
import Block.Internal.Types exposing (..)
import Block.Internal.View.BodyModel
import Block.Internal.View.Model as ViewModel exposing (ViewModel)
import Delta exposing (Delta)
import DragState exposing (DragState)
import Grid
import MathEx
import Pair
import Pos exposing (Pos)
import Svg
import Svg.Attributes as SvgAttrs



-- VIEW


view : List (Svg.Attribute msg) -> ViewModel -> Maybe (Svg.Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging Component.Width _ ->
            Just (viewControl attrs vm)

        Selected ->
            Just (viewControl attrs vm)

        _ ->
            Nothing


viewControl : List (Svg.Attribute msg) -> ViewModel -> Svg.Svg msg
viewControl attrs vm =
    let
        { radius, pos, cpos } =
            case vm.block.state of
                Dragging Component.Width drag ->
                    { radius = round (vm.grid.unit / 1.2)
                    , pos =
                        drag.start
                            |> Pos.addDelta drag.delta.current
                    , cpos =
                        drag.start
                            |> circlePos vm.grid.unit vm.block.size.height
                            |> Pos.addDelta drag.delta.total
                    }

                _ ->
                    let
                        p =
                            rootPos vm
                    in
                    { radius = round (vm.grid.unit / 1.5)
                    , pos = p
                    , cpos = p |> circlePos vm.grid.unit vm.block.size.height
                    }

        vbarP1 =
            pos

        vbarP2 =
            vbarP1 |> Pos.addDelta (Delta.init ( 0, vm.block.size.height ))

        hbarP1 =
            vbarP1 |> Pos.addDelta (Delta.init ( 0, vm.block.size.height / 2 ))

        hbarP2 =
            Pos.init ( cpos.x, hbarP1.y )

        ( guideP1, guideP2 ) =
            ( Pos.init ( cpos.x, vm.grid.pos.y )
            , Pos.init ( cpos.x, vm.grid.pos.y + vm.grid.size.height )
            )
    in
    Svg.g
        (SvgAttrs.class "width-control" :: attrs)
        [ Svg.line
            [ SvgAttrs.x1 <| Pos.toXString guideP1
            , SvgAttrs.y1 <| Pos.toYString guideP1
            , SvgAttrs.x2 <| Pos.toXString guideP2
            , SvgAttrs.y2 <| Pos.toYString guideP2
            , SvgAttrs.strokeWidth <| String.fromFloat <| guideLineWidth
            , SvgAttrs.strokeDasharray "4"
            ]
            []
        , Svg.line
            [ SvgAttrs.x1 <| Pos.toXString vbarP1
            , SvgAttrs.y1 <| Pos.toYString vbarP1
            , SvgAttrs.x2 <| Pos.toXString vbarP2
            , SvgAttrs.y2 <| Pos.toYString vbarP2
            , SvgAttrs.strokeWidth <| String.fromFloat <| lineWidth
            ]
            []
        , Svg.line
            [ SvgAttrs.x1 <| Pos.toXString hbarP1
            , SvgAttrs.y1 <| Pos.toYString hbarP1
            , SvgAttrs.x2 <| Pos.toXString hbarP2
            , SvgAttrs.y2 <| Pos.toYString hbarP2
            , SvgAttrs.strokeWidth <| String.fromFloat <| lineWidth
            ]
            []
        , Svg.circle
            [ SvgAttrs.cx <| Pos.toXString cpos
            , SvgAttrs.cy <| Pos.toYString cpos
            , SvgAttrs.r <| String.fromInt <| radius
            ]
            []
        ]


lineWidth : Float
lineWidth =
    3


guideLineWidth : Float
guideLineWidth =
    3


circlePos : Float -> Float -> Pos -> Pos
circlePos unit blockHeight pos =
    pos |> Pos.addDelta (Delta unit (blockHeight / 2))


rootPos : ViewModel -> Pos
rootPos vm =
    let
        root =
            Maybe.withDefault vm.body.mid vm.body.top

        delta =
            Delta.init
                ( vm.block.size.width + lineWidth
                , 0
                )
    in
    Pos.addDelta delta root.pos



-- UPDATE


startDrag : ViewModel -> Block -> DragState Block
startDrag vm bd =
    DragState.init
        { start = rootPos vm
        , data = bd
        , addFn = Delta.addX
        }



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
    in
    { bd | width = width_ }


dragEnd : DragState Block -> Grid.Data -> Block -> Maybe Block
dragEnd _ _ bd =
    Just bd
