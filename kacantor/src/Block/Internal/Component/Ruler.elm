module Block.Internal.Component.Ruler exposing (..)

import Block.Internal.Section as Section exposing (Section)
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel2)
import Line exposing (Line)
import Maybe.Extra
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel2 -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging _ (DragQuantity _) ->
            Just (viewRulers attrs vm)

        Dragging _ (DragWidth _) ->
            Just (viewRulers attrs vm)

        _ ->
            Nothing


viewRulers : List (Attribute msg) -> ViewModel2 -> Svg msg
viewRulers attrs vm =
    let
        widthRuler =
            viewWidthRuler vm

        heightRuler =
            vm.sections
                |> Section.midSection
                |> Maybe.map (viewHeightRuler vm)
                |> Maybe.Extra.toList
    in
    Svg.g
        (SvgAttrs.class "ruler" :: attrs)
        (widthRuler :: heightRuler)


viewWidthRuler : ViewModel2 -> Svg msg
viewWidthRuler vm =
    let
        ( halfUnit, quarterUnit ) =
            ( vm.grid.unit / 2, vm.grid.unit / 4 )

        line =
            vm.pos
                |> Pos.addY -halfUnit
                |> Line.addX vm.size.width

        hash1 =
            Line
                (line.p1 |> Pos.addY quarterUnit)
                (line.p1 |> Pos.addY -quarterUnit)

        hash2 =
            Line
                (line.p2 |> Pos.addY quarterUnit)
                (line.p2 |> Pos.addY -quarterUnit)

        txt =
            (vm.size.width / vm.grid.unit)
                |> round
                |> String.fromInt

        txtSize =
            Size.forSquare halfUnit

        txtPos =
            line.p1
                |> Pos.addX (vm.size.width / 2)
                |> Pos.addX -(txtSize.width / 2)
                |> Pos.addY -(txtSize.height / 2)
    in
    viewRuler
        { class = "height-ruler"
        , hash1 = hash1
        , hash2 = hash2
        , line = line
        , txt =
            { val = txt
            , pos = txtPos
            , size = txtSize
            }
        }


viewHeightRuler : ViewModel2 -> Section -> Svg msg
viewHeightRuler vm mid =
    let
        ( halfUnit, quarterUnit ) =
            ( vm.grid.unit / 2, vm.grid.unit / 4 )

        line =
            mid.pos
                |> Pos.addX -halfUnit
                |> Line.addY mid.size.height

        hash1 =
            Line
                (line.p1 |> Pos.addX quarterUnit)
                (line.p1 |> Pos.addX -quarterUnit)

        hash2 =
            Line
                (line.p2 |> Pos.addX quarterUnit)
                (line.p2 |> Pos.addX -quarterUnit)

        txt =
            (mid.size.height / vm.grid.unit)
                |> round
                |> String.fromInt

        txtSize =
            Size.forSquare halfUnit

        txtPos =
            line.p1
                |> Pos.addY (mid.size.height / 2)
                |> Pos.addX -(txtSize.width / 2)
                |> Pos.addY -(txtSize.height / 2)
    in
    viewRuler
        { class = "height-ruler"
        , hash1 = hash1
        , hash2 = hash2
        , line = line
        , txt =
            { val = txt
            , pos = txtPos
            , size = txtSize
            }
        }


viewRuler :
    { class : String
    , hash1 : Line
    , hash2 : Line
    , line : Line
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
        [ SvgEx.line [] input.line
        , SvgEx.line [] input.hash1
        , SvgEx.line [] input.hash2
        , SvgEx.rect [] input.txt
        , SvgEx.centeredText
            []
            input.txt.pos
            input.txt.size
            input.txt.val
        ]
