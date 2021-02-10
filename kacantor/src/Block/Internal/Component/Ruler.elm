module Block.Internal.Component.Ruler exposing (..)

import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel)
import Delta exposing (Delta)
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        -- Dragging _ _ ->
        --     Just (viewOutline attrs vm)
        -- Selected ->
        --     Just (viewOutline attrs vm)
        _ ->
            Nothing


viewOutline : List (Attribute msg) -> ViewModel -> Svg msg
viewOutline attrs vm =
    let
        root =
            rootPosition vm

        ( hlineP1, hlineP2 ) =
            root |> hLinePositions vm

        ( leftP1, leftP2 ) =
            hlineP1 |> vlinePositions vm

        ( rightP1, rightP2 ) =
            hlineP2 |> vlinePositions vm

        ( txtPos, txtSize ) =
            root |> txtPositionAndSize vm
    in
    Svg.g
        (SvgAttrs.class "ruler" :: attrs)
        [ Svg.line
            [ SvgAttrs.x1 <| Pos.toXString hlineP1
            , SvgAttrs.y1 <| Pos.toYString hlineP1
            , SvgAttrs.x2 <| Pos.toXString hlineP2
            , SvgAttrs.y2 <| Pos.toYString hlineP2
            ]
            []
        , Svg.line
            [ SvgAttrs.x1 <| Pos.toXString leftP1
            , SvgAttrs.y1 <| Pos.toYString leftP1
            , SvgAttrs.x2 <| Pos.toXString leftP2
            , SvgAttrs.y2 <| Pos.toYString leftP2
            ]
            []
        , Svg.line
            [ SvgAttrs.x1 <| Pos.toXString rightP1
            , SvgAttrs.y1 <| Pos.toYString rightP1
            , SvgAttrs.x2 <| Pos.toXString rightP2
            , SvgAttrs.y2 <| Pos.toYString rightP2
            ]
            []
        , Svg.rect
            [ SvgAttrs.x <| Pos.toXString txtPos
            , SvgAttrs.y <| Pos.toYString txtPos
            , SvgAttrs.width <| Size.toWidthString txtSize
            , SvgAttrs.height <| Size.toHeightString txtSize
            ]
            []
        , SvgEx.centeredText
            [ SvgAttrs.class "ruler-text" ]
            txtPos
            txtSize
            (String.fromInt vm.block.width)
        ]


rootOffset : ViewModel -> Float
rootOffset vm =
    vm.grid.unit / 2


rootPosition : ViewModel -> Pos
rootPosition vm =
    vm.block.pos
        |> Pos.addX (vm.block.size.width / 2)
        |> Pos.addY -(rootOffset vm)


hLinePositions : ViewModel -> Pos -> ( Pos, Pos )
hLinePositions vm root =
    let
        halfWidth =
            vm.block.size.width / 2
    in
    ( root |> Pos.addDelta (Delta -halfWidth 0)
    , root |> Pos.addDelta (Delta halfWidth 0)
    )


vlinePositions : ViewModel -> Pos -> ( Pos, Pos )
vlinePositions vm pos =
    let
        yDelta =
            rootOffset vm / 2
    in
    ( pos |> Pos.addY yDelta
    , pos |> Pos.addY -yDelta
    )


txtPositionAndSize : ViewModel -> Pos -> ( Pos, Size )
txtPositionAndSize vm root =
    let
        offset =
            rootOffset vm / 2

        pos =
            root |> Pos.addDelta (Delta -offset -offset)

        size =
            Size offset offset |> Size.scale 2
    in
    ( pos, size )
