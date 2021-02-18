module Block.Internal.Component.Ruler exposing (..)

import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel)
import Delta exposing (Delta)
import Maybe.Extra
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg, line)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging2 _ (DragQuantity _) ->
            Just (viewOutline attrs vm)

        Dragging2 _ (DragWidth _) ->
            Just (viewOutline attrs vm)

        _ ->
            Nothing


viewOutline : List (Attribute msg) -> ViewModel -> Svg msg
viewOutline attrs vm =
    let
        optional =
            [ viewHeightRuler vm ] |> Maybe.Extra.values
    in
    Svg.g
        (SvgAttrs.class "ruler" :: attrs)
        (viewWidthRuler attrs vm :: optional)


viewWidthRuler : List (Attribute msg) -> ViewModel -> Svg msg
viewWidthRuler attrs vm =
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


viewHeightRuler : ViewModel -> Maybe (Svg msg)
viewHeightRuler vm =
    let
        { unit, halfUnit, quarterUnit } =
            { unit = vm.grid.unit
            , halfUnit = vm.grid.unit / 2
            , quarterUnit = vm.grid.unit / 4
            }

        height =
            vm.body.mid.size.height / unit

        lineP1 =
            vm.body.mid.pos
                |> Pos.addX -halfUnit

        lineP2 =
            lineP1 |> Pos.addY vm.body.mid.size.height

        hash1 =
            ( lineP1 |> Pos.addX -quarterUnit
            , lineP1 |> Pos.addX quarterUnit
            )

        hash2 =
            ( lineP2 |> Pos.addX -quarterUnit
            , lineP2 |> Pos.addX quarterUnit
            )

        txtSize =
            Size halfUnit halfUnit

        txtPos =
            lineP1
                |> Pos.addY (vm.body.mid.size.height / 2)
                |> Pos.addX -(txtSize.width / 2)
                |> Pos.addY -(txtSize.height / 2)
    in
    if height < 3 then
        Nothing

    else
        Just <|
            viewRuler
                { class = "height-ruler"
                , hash1 = hash1
                , hash2 = hash2
                , line = ( lineP1, lineP2 )
                , txt =
                    { val = String.fromInt <| round height
                    , pos = txtPos
                    , size = txtSize
                    }
                }


viewRuler :
    { class : String
    , hash1 : ( Pos, Pos )
    , hash2 : ( Pos, Pos )
    , line : ( Pos, Pos )
    , txt :
        { val : String
        , pos : Pos
        , size : Size
        }
    }
    -> Svg msg
viewRuler input =
    Svg.g
        [ SvgAttrs.class input.class ]
        [ Svg.line
            [ SvgAttrs.x1 <| Pos.toXString <| Tuple.first input.line
            , SvgAttrs.y1 <| Pos.toYString <| Tuple.first input.line
            , SvgAttrs.x2 <| Pos.toXString <| Tuple.second input.line
            , SvgAttrs.y2 <| Pos.toYString <| Tuple.second input.line
            ]
            []
        , Svg.line
            [ SvgAttrs.x1 <| Pos.toXString <| Tuple.first input.hash1
            , SvgAttrs.y1 <| Pos.toYString <| Tuple.first input.hash1
            , SvgAttrs.x2 <| Pos.toXString <| Tuple.second input.hash1
            , SvgAttrs.y2 <| Pos.toYString <| Tuple.second input.hash1
            ]
            []
        , Svg.line
            [ SvgAttrs.x1 <| Pos.toXString <| Tuple.first input.hash2
            , SvgAttrs.y1 <| Pos.toYString <| Tuple.first input.hash2
            , SvgAttrs.x2 <| Pos.toXString <| Tuple.second input.hash2
            , SvgAttrs.y2 <| Pos.toYString <| Tuple.second input.hash2
            ]
            []
        , Svg.rect
            [ SvgAttrs.x <| Pos.toXString input.txt.pos
            , SvgAttrs.y <| Pos.toYString input.txt.pos
            , SvgAttrs.width <| Size.toWidthString input.txt.size
            , SvgAttrs.height <| Size.toHeightString input.txt.size
            ]
            []
        , SvgEx.centeredText
            []
            input.txt.pos
            input.txt.size
            input.txt.val
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
