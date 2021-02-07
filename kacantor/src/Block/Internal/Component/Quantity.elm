module Block.Internal.Component.Quantity exposing (..)

import Block.Internal.Component as Component
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel)
import Delta
import DragState exposing (DragState)
import Grid
import Pair
import Pos exposing (Pos)
import Size exposing (Size)
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
            pos

        rectSize =
            Size unit unit

        vlineP1 =
            Pos.add rectPos <| Pos.init ( halfUnit, rectSize.height )

        vlineP2 =
            Pos.addY vlineP1 <| Pos.init ( 0, unit )

        circlePos =
            vlineP2
    in
    Svg.g
        [ SvgAttrs.class "quantity-control" ]
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
            (attrs
                ++ [ SvgAttrs.cx <| Pos.toXString circlePos
                   , SvgAttrs.cy <| Pos.toYString circlePos
                   , SvgAttrs.r <| String.fromInt <| radius
                   ]
            )
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
        pos =
            drag.pos.total
                |> Pos.roundNear { pos = Pos.fromInt ( gd.x, gd.y ), unit = toFloat gd.unit }

        ( dx, dy ) =
            pos
                |> Pos.deltaBetween bd.pos
                |> Delta.div (toFloat gd.unit)
                |> Delta.map (max 0)
                |> Pair.map round

        quantity_ =
            (dy * bd.width) + min dx bd.width - bd.headerOffset
    in
    { bd | quantity = quantity_ }


dragEnd : DragState Block -> Grid.Data -> Block -> Maybe Block
dragEnd _ _ bd =
    if bd.quantity <= 0 then
        Nothing

    else
        Just bd
