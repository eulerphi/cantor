module CircleButton exposing (..)

import Box exposing (Box)
import Delta exposing (Delta)
import Grid
import Pos
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> Grid.Data -> String -> Svg msg
view attrs gd txt =
    let
        ( gridPos, gridSize, unit ) =
            ( Pos.fromInt ( gd.x, gd.y )
            , Size.fromInt ( gd.width, gd.height )
            , toFloat gd.unit
            )

        ( doubleUnit, threeHalfsUnit, threeFourthsUnit ) =
            ( 2 * unit, 3 * unit / 2, 3 * unit / 4 )

        ( btnPos, btnRadius ) =
            ( gridPos |> Pos.addDelta (Delta doubleUnit (gridSize.height - doubleUnit))
            , unit
            )

        ( btnTxtPos, btnTxtSize ) =
            ( btnPos |> Pos.addDelta (Delta -threeFourthsUnit -threeFourthsUnit)
            , Size threeHalfsUnit threeHalfsUnit
            )
    in
    Svg.g
        attrs
        [ Svg.circle
            [ SvgAttrs.cx <| Pos.toXString btnPos
            , SvgAttrs.cy <| Pos.toYString btnPos
            , SvgAttrs.r <| String.fromFloat btnRadius
            ]
            []
        , SvgEx.text_
            []
            (Box btnTxtPos btnTxtSize)
            txt
        ]
