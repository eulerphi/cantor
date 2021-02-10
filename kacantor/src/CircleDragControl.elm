module CircleDragControl exposing (..)

import Pos exposing (Pos)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs


view :
    List (Attribute msg)
    ->
        { active : Bool
        , pos : Pos
        , unit : Float
        }
    -> Svg msg
view attrs input =
    Svg.g
        (SvgAttrs.class "drag-control" :: attrs)
        [ Svg.circle
            [ SvgAttrs.cx <| Pos.toXString input.pos
            , SvgAttrs.cy <| Pos.toYString input.pos
            , SvgAttrs.r <| String.fromFloat <| controlRadius input
            ]
            []
        , Svg.circle
            [ SvgAttrs.class "grip"
            , SvgAttrs.cx <| Pos.toXString input.pos
            , SvgAttrs.cy <| Pos.toYString input.pos
            , SvgAttrs.r <| String.fromFloat <| gripRadius input
            ]
            []
        ]


controlRadius : { r | active : Bool, unit : Float } -> Float
controlRadius { active, unit } =
    if active then
        unit / 1.2

    else
        unit / 3


gripRadius : { r | active : Bool, unit : Float } -> Float
gripRadius { active, unit } =
    if active then
        unit / 1.2

    else
        unit / 1.5
