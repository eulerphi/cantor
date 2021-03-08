module Block.Internal.Component.Multiplier exposing (..)

import Block.Internal.Section as Section exposing (Section)
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel exposing (ViewModel)
import Box exposing (Box)
import CircleDragControl as CircleControl
import Delta exposing (Delta)
import DragState exposing (DragState)
import Grid exposing (Grid)
import Line exposing (Line)
import Maybe.Extra
import OffsetAnchor
import Pair
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> ViewModel -> Maybe (Svg msg)
view attrs vm =
    case vm.block.state of
        Dragging _ (MultiplierDrag state) ->
            { active = True
            , rpos = rootPosition vm
            , cpos = state.current
            }
                |> viewControl attrs vm
                |> Just

        Selected ->
            rootPosition vm
                |> Pair.fork
                    identity
                    (circlePosition vm)
                |> (\( rpos, cpos ) -> { active = False, rpos = rpos, cpos = cpos })
                |> viewControl attrs vm
                |> Just

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


viewControl :
    List (Attribute msg)
    -> ViewModel
    -> { active : Bool, rpos : Pos, cpos : Pos }
    -> Svg msg
viewControl attrs vm { active, rpos, cpos } =
    let
        quarterUnit =
            vm.grid.unit / 4

        txt =
            (vm.block.quantity // vm.block.width)
                |> String.fromInt
    in
    Svg.g
        [ SvgAttrs.class "multiplier" ]
        [ SvgEx.line
            []
            (Line rpos cpos)
        , SvgEx.line
            [ SvgAttrs.class "guideline" ]
            (rpos |> Line.toY (OffsetAnchor.toY OffsetAnchor.Bottom vm.grid))
        , SvgEx.line
            []
            (rpos |> Line.centeredX quarterUnit)
        , CircleControl.view2
            attrs
            { active = active
            , pos = cpos
            , unit = vm.grid.unit
            , txt = txt
            }
        ]


viewHeightRuler : ViewModel -> Section -> Svg msg
viewHeightRuler vm mid =
    let
        ( halfUnit, quarterUnit ) =
            ( vm.grid.unit / 2, vm.grid.unit / 4 )

        filled =
            mid.pos
                |> Pos.addX -(3 * vm.grid.unit / 4)
                |> Line.addY mid.size.height

        guide =
            filled.p2
                |> Line.toY (OffsetAnchor.toY OffsetAnchor.Bottom vm.grid)

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
        , filled = filled
        , guide = guide
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
    , filled : Line
    , guide : Line
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
            [ SvgEx.line [] input.filled
            , SvgEx.line [ SvgAttrs.class "guideline" ] input.guide
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


rootPosition : ViewModel -> Pos
rootPosition vm =
    vm.pos |> Pos.addX -(3 * vm.grid.unit / 4)


circlePosition : ViewModel -> Pos -> Pos
circlePosition vm rpos =
    vm.sections
        |> Section.midSection
        |> Maybe.map (\s -> s.size.height)
        |> Maybe.withDefault 0
        |> (\y -> Pos.addY y rpos)


dragStart : ViewModel -> Maybe DragState
dragStart vm =
    vm
        |> rootPosition
        |> circlePosition vm
        |> DragState.forStart
        |> Just


dragUpdate : Delta -> DragState -> DragState
dragUpdate delta drag =
    drag |> DragState.update Delta.addY delta


dragMove : DragContext -> DragState -> Block
dragMove { gd, bd } state =
    let
        dy =
            state.delta
                |> Delta.roundNear gd.unit
                |> Delta.div gd.unit
                |> .dy
                |> round

        remainder =
            modBy bd.width bd.quantity

        quantity_ =
            max remainder (bd.quantity + (bd.width * dy))
    in
    { bd | quantity = quantity_ }


dragEnd : DragContext -> DragState -> Maybe Block
dragEnd ctx state =
    dragMove ctx state |> Just
