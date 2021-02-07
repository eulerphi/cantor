module SvgEx exposing
    ( centeredText
    , translateToPos
    )

import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs


centeredText : List (Attribute msg) -> Pos -> Size -> String -> Svg msg
centeredText attrs pos size text =
    Svg.g
        (translateToPos pos :: attrs)
        [ Svg.svg
            [ SvgAttrs.width <| Size.toWidthString size
            , SvgAttrs.height <| Size.toHeightString size
            ]
            [ Svg.text_
                [ SvgAttrs.x "50%"
                , SvgAttrs.y "50%"
                , SvgAttrs.fill "black"
                , SvgAttrs.dominantBaseline "middle"
                , SvgAttrs.textAnchor "middle"
                ]
                [ Svg.text text ]
            ]
        ]


translateToPos : Pos -> Attribute msg
translateToPos pos =
    let
        ( x, y ) =
            ( pos |> Pos.toXString
            , pos |> Pos.toYString
            )
    in
    SvgAttrs.transform <| "translate(" ++ x ++ " " ++ y ++ ")"
