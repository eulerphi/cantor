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
        { radius, cpos, delta } =
            case vm.block.state of
                Dragging Component.Width drag ->
                    { radius = round (vm.grid.unit / 1.2)
                    , cpos = drag.pos.total
                    , delta = drag.delta.total
                    }

                _ ->
                    { radius = round (vm.grid.unit / 1.5)
                    , cpos = rootPos vm
                    , delta = Delta.none
                    }

        pos =
            rootPos vm

        p1 =
            pos |> Pos.addDelta delta

        p2 =
            p1 |> Pos.add (Pos.init ( -vm.grid.unit, 0 ))

        -- p1 =
        -- p2 =
        --     Pos.addY p1 { x = 0.0, y = vm.block.size.height }
    in
    Svg.g
        (SvgAttrs.class "width-control" :: attrs)
        [ Svg.line
            [ SvgAttrs.x1 <| Pos.toXString p1
            , SvgAttrs.y1 <| Pos.toYString p1
            , SvgAttrs.x2 <| Pos.toXString p2
            , SvgAttrs.y2 <| Pos.toYString p2
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


rootPos : ViewModel -> Pos
rootPos vm =
    let
        root =
            Maybe.withDefault vm.body.mid vm.body.top

        delta =
            Delta.init
                ( vm.block.size.width + vm.grid.unit + lineWidth
                , vm.grid.unit / 2
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
dragMove drag _ bd =
    bd


dragEnd : DragState Block -> Grid.Data -> Block -> Maybe Block
dragEnd drag gd bd =
    Just bd
