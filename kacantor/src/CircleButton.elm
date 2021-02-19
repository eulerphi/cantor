module CircleButton exposing (..)

import Box exposing (Box)
import Delta exposing (Delta)
import Grid exposing (Grid)
import Pos
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : List (Attribute msg) -> Grid -> String -> Svg msg
view attrs gd txt =
    let
        ( doubleUnit, threeHalfsUnit, threeFourthsUnit ) =
            ( 2 * gd.unit, 3 * gd.unit / 2, 3 * gd.unit / 4 )

        ( btnPos, btnRadius ) =
            ( gd.pos |> Pos.addDelta (Delta doubleUnit (gd.size.height - doubleUnit))
            , threeFourthsUnit
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
