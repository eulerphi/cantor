module Block.Internal.Component.Multiplicand exposing (..)

import Block.Internal.Section as Section exposing (Section)
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel)
import Box exposing (Box)
import CircleDragControl as CircleControl
import Delta exposing (Delta)
import DragState
import Grid exposing (Grid)
import Line exposing (Line)
import Maybe.Extra
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view attrs vm =
    let
        viewFn =
            viewWidthRuler vm
                |> (\el -> Svg.g (SvgAttrs.class "multiplicand" :: attrs) [ el ])
                |> Just
    in
    case vm.block.state of
        Dragging _ _ ->
            viewFn

        Selected ->
            viewFn

        _ ->
            Nothing


viewWidthRuler : ViewModel -> Svg msg
viewWidthRuler vm =
    let
        ( halfUnit, quarterUnit ) =
            ( vm.grid.unit / 2, vm.grid.unit / 4 )

        line =
            vm.pos
                |> Pos.addY -(3 * vm.grid.unit / 4)
                |> Line.addX (vm.grid.size.width - vm.pos.x)

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
        , line = line
        , cpos = line.p1 |> Pos.addX vm.size.width
        , txt =
            { val = txt
            , pos = txtPos
            , size = txtSize
            }
        }


viewRuler :
    { class : String
    , line : Line
    , hash1 : Line
    , cpos : Pos
    , txt :
        { val : String
        , pos : Pos
        , size : Size
        }
    }
    -> Svg msg
viewRuler input =
    let
        es =
            [ SvgEx.line [ SvgAttrs.strokeDasharray "6", SvgAttrs.opacity "30%" ] input.line
            , SvgEx.line [] input.hash1
            , CircleControl.view2 [] { active = False, pos = input.cpos, unit = 33, txt = input.txt.val }
            ]

        txt =
            if input.txt.val /= "0" then
                [ SvgEx.textWithBackground [] input.txt input.txt.val ]

            else
                []
    in
    Svg.g
        [ SvgAttrs.class input.class ]
        es
