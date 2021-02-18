module SvgEx exposing
    ( centeredText
    , horizontalGuideline
    , line
    , rect
    , translateToPos
    , verticalGuideline
    )

import Box exposing (Boxlike)
import Line exposing (Line, Linelike)
import Pair
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


line : List (Attribute msg) -> Linelike r -> Svg msg
line attrs { p1, p2 } =
    let
        lineAttrs =
            [ SvgAttrs.x1 <| Pos.toXString p1
            , SvgAttrs.y1 <| Pos.toYString p1
            , SvgAttrs.x2 <| Pos.toXString p2
            , SvgAttrs.y2 <| Pos.toYString p2
            ]
    in
    Svg.line
        (lineAttrs ++ attrs)
        []


horizontalGuideline : List (Attribute msg) -> Boxlike r -> Pos -> Svg msg
horizontalGuideline attrs box pos =
    pos
        |> Pos.updateX box.pos.x
        |> Pair.fork
            identity
            (Pos.addX box.size.width)
        |> Pair.uncurry Line
        |> line (SvgAttrs.class "guideline" :: attrs)


verticalGuideline : List (Attribute msg) -> Boxlike r -> Pos -> Svg msg
verticalGuideline attrs box pos =
    pos
        |> Pos.updateY box.pos.y
        |> Pair.fork
            identity
            (Pos.addY box.size.height)
        |> Pair.uncurry Line
        |> line (SvgAttrs.class "guideline" :: attrs)


rect : List (Attribute msg) -> Boxlike r -> Svg msg
rect attrs { pos, size } =
    let
        boxAttrs =
            [ SvgAttrs.x <| Pos.toXString pos
            , SvgAttrs.y <| Pos.toYString pos
            , SvgAttrs.width <| Size.toWidthString size
            , SvgAttrs.height <| Size.toHeightString size
            ]
    in
    Svg.rect
        (attrs ++ boxAttrs)
        []


translateToPos : Pos -> Attribute msg
translateToPos pos =
    let
        ( x, y ) =
            ( pos |> Pos.toXString
            , pos |> Pos.toYString
            )
    in
    SvgAttrs.transform <| "translate(" ++ x ++ " " ++ y ++ ")"
