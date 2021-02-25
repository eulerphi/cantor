module SvgEx exposing
    ( foreignObject
    , horizontalGuideline
    , line
    , rect
    , textWithBackground
    , text_
    , translateToPos
    , verticalGuideline
    )

import Box exposing (Boxlike)
import Html exposing (Html)
import Line exposing (Line, Linelike)
import Pair
import Pos exposing (Pos)
import Size
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs


foreignObject :
    List (Attribute msg)
    -> Boxlike r
    -> List (Html msg)
    -> Svg msg
foreignObject attrs box content =
    Svg.foreignObject
        ([ SvgAttrs.x <| Pos.toXString box.pos
         , SvgAttrs.y <| Pos.toYString box.pos
         , SvgAttrs.width <| Size.toWidthString box.size
         , SvgAttrs.height <| Size.toHeightString box.size
         ]
            ++ attrs
        )
        content


text_ : List (Attribute msg) -> Boxlike r -> String -> Svg msg
text_ attrs box text =
    Svg.g
        (translateToPos box.pos :: attrs)
        [ Svg.svg
            [ SvgAttrs.width <| Size.toWidthString box.size
            , SvgAttrs.height <| Size.toHeightString box.size
            ]
            [ Svg.text_
                [ SvgAttrs.x "50%"
                , SvgAttrs.y "50%"
                , SvgAttrs.dominantBaseline "middle"
                , SvgAttrs.textAnchor "middle"
                ]
                [ Svg.text text ]
            ]
        ]


textWithBackground : List (Attribute msg) -> Boxlike r -> String -> Svg msg
textWithBackground attrs box text =
    Svg.g
        attrs
        [ rect [] box
        , Svg.g
            (translateToPos box.pos :: attrs)
            [ Svg.svg
                [ SvgAttrs.width <| Size.toWidthString box.size
                , SvgAttrs.height <| Size.toHeightString box.size
                ]
                [ Svg.text_
                    [ SvgAttrs.x "50%"
                    , SvgAttrs.y "50%"
                    , SvgAttrs.dominantBaseline "middle"
                    , SvgAttrs.textAnchor "middle"
                    ]
                    [ Svg.text text ]
                ]
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
