module Block.Internal.Component.Multiplier exposing (..)

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
            vm.sections
                |> Section.midSection
                |> Maybe.withDefault (emptyMidSection vm)
                |> viewHeightRuler vm
                |> (\el -> Svg.g (SvgAttrs.class "multiple" :: attrs) [ el ])
                |> Just
    in
    case vm.block.state of
        Dragging _ _ ->
            viewFn

        Selected ->
            viewFn

        _ ->
            Nothing


emptyMidSection : ViewModel -> Section
emptyMidSection vm =
    { pos = vm.pos
    , size = Size.none
    , sizeInUnits = Size.noneInt
    , isMid = True
    , offset = 0
    , class = ""
    , quantity = 0
    }


viewHeightRuler : ViewModel -> Section -> Svg msg
viewHeightRuler vm mid =
    let
        ( halfUnit, quarterUnit ) =
            ( vm.grid.unit / 2, vm.grid.unit / 4 )

        line =
            mid.pos
                |> Pos.addX -(3 * vm.grid.unit / 4)
                |> Line.addY (vm.grid.size.height - vm.pos.y)

        hash1 =
            Line
                (line.p1 |> Pos.addX quarterUnit)
                (line.p1 |> Pos.addX -quarterUnit)

        hash2 =
            Line
                (line.p2 |> Pos.addX halfUnit)
                (line.p2 |> Pos.addX -halfUnit)

        txt =
            (mid.size.height / vm.grid.unit)
                |> round
                |> String.fromInt

        txtSize =
            Size.forSquare (3 * vm.grid.unit / 4)

        txtPos =
            line.p2
                |> Pos.addX -(txtSize.width / 2)
                |> Pos.addY -(mid.size.height / 2)
                |> Pos.addY -(txtSize.height / 2)
    in
    viewRuler
        { class = "height-ruler"
        , hash1 = hash1
        , line = line
        , cpos = line.p1 |> Pos.addY mid.size.height
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

        -- txt =
        --     if input.txt.val /= "0" then
        --         [ SvgEx.textWithBackground [] input.txt input.txt.val ]
        --     else
        --         []
    in
    Svg.g
        [ SvgAttrs.class input.class ]
        es



-- (es ++ txt)
