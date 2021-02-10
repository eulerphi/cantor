module Block.Internal.Component.Body exposing (..)

import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel)
import Delta
import DragState exposing (DragState)
import Grid
import List
import Maybe.Extra
import Pair
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx
import ViewData exposing (ViewData)



-- VIEW


view : List (Attribute msg) -> ViewModel -> Svg msg
view eventAttrs vm =
    let
        viewFn =
            viewRect vm

        optional =
            [ vm.body.top, vm.body.bot ]
                |> List.map (Maybe.map viewFn)

        elements =
            (Just (viewFn vm.body.mid) :: optional)
                |> Maybe.Extra.values
    in
    Svg.g
        (SvgAttrs.class "block-body" :: eventAttrs)
        elements


viewRect : ViewModel -> ViewData -> Svg.Svg msg
viewRect vm vd =
    let
        grid =
            Grid.forViewData (round vm.grid.unit) vd

        gridElement =
            Grid.view [] grid

        -- hack time : need to refactor ViewData to take a generic
        ( cols, rows ) =
            vd.size
                |> Size.scale (1 / vm.grid.unit)
                |> Size.toPair
                |> Pair.map round

        -- disks =
        --     viewDisks vm vd ( cols, rows )
        txt =
            viewTxt ( cols, rows ) vm vd
    in
    Svg.g
        [ SvgAttrs.class vd.class ]
        [ gridElement, txt ]


viewDisks : ViewModel -> ViewData -> ( Int, Int ) -> List (Svg msg)
viewDisks vm vd ( cols, rows ) =
    diskIndexes ( cols, rows ) |> List.map (viewDisk vm vd ( cols, rows ))


viewDisk : ViewModel -> ViewData -> ( Int, Int ) -> ( Int, Int ) -> Svg msg
viewDisk vm vd ( cols, rows ) ( colIdx, rowIdx ) =
    let
        ( unit, halfUnit ) =
            ( vm.grid.unit, vm.grid.unit / 2 )

        pos =
            Pos.fromInt ( colIdx, rowIdx )
                |> Pos.scale unit
                |> Pos.add (Pos -halfUnit -halfUnit)
                |> Pos.add vd.pos

        radius =
            unit / 2.5
    in
    Svg.circle
        [ SvgAttrs.class <| diskClass cols colIdx
        , SvgAttrs.cx <| Pos.toXString pos
        , SvgAttrs.cy <| Pos.toYString pos
        , SvgAttrs.r <| String.fromFloat <| radius
        ]
        []


diskClass : Int -> Int -> String
diskClass cols colIdx =
    let
        tens =
            10 * (cols // 10)

        fives =
            tens + 5 * ((cols - tens) // 5)
    in
    if colIdx <= tens then
        "disk-10"

    else if colIdx <= fives then
        "disk-5"

    else
        "disk-" ++ (String.fromInt <| cols - fives)


diskIndexes : ( Int, Int ) -> List ( Int, Int )
diskIndexes ( cols, rows ) =
    let
        inner =
            \r ->
                List.range 1 cols |> List.map (\c -> ( c, r ))
    in
    List.range 1 rows |> List.map inner |> List.concat


viewTxt : ( Int, Int ) -> ViewModel -> ViewData -> Svg msg
viewTxt ( cols, rows ) vm vd =
    let
        quantity =
            vm.block.width * (rows - 1) + cols
    in
    SvgEx.centeredText
        [ SvgAttrs.class (vd.class ++ "-text") ]
        vd.pos
        (Size vm.grid.unit vm.grid.unit)
        (String.fromInt quantity)



-- UPDATE


startDrag : ViewModel -> Block -> DragState Block
startDrag vm bd =
    DragState.init
        { start = vm.block.pos
        , data = bd
        , addFn = Delta.add
        }


dragMove : DragState Block -> Grid.Data -> Block -> Block
dragMove drag _ bd =
    { bd | pos = drag.pos.current }


dragEnd : DragState Block -> Grid.Data -> Block -> Maybe Block
dragEnd drag gd bd =
    let
        pos_ =
            Pos.roundNear
                { pos = Pos.fromInt ( gd.x, gd.y )
                , unit = toFloat gd.unit
                }
                drag.pos.current
    in
    Just { bd | pos = pos_ }
