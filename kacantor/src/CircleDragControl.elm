module CircleDragControl exposing (..)

import Box exposing (Box)
import OffsetAnchor exposing (HorizontalAnchor(..), OffsetAnchor(..), VerticalAnchor(..))
import Pair
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view :
    List (Attribute msg)
    ->
        { active : Bool
        , pos : Pos
        , unit : Float
        }
    -> Svg msg
view attrs input =
    view2 attrs { active = input.active, pos = input.pos, unit = input.unit, txt = "" }


view2 :
    List (Attribute msg)
    ->
        { active : Bool
        , pos : Pos
        , unit : Float
        , txt : String
        }
    -> Svg msg
view2 attrs input =
    view3 attrs
        { active = input.active
        , offsetAnchor = OffsetAnchor HCenter VCenter
        , pos = input.pos
        , unit = input.unit
        , txt = input.txt
        }


view3 :
    List (Attribute msg)
    ->
        { active : Bool
        , offsetAnchor : OffsetAnchor
        , pos : Pos
        , unit : Float
        , txt : String
        }
    -> Svg msg
view3 attrs input =
    let
        ( cr, gr ) =
            input |> Pair.fork controlRadius gripRadius

        pos_ =
            input.pos
                |> Pos.addXY -cr
                |> (\p -> Box p (Size.square (2 * cr)))
                |> OffsetAnchor.toPos input.offsetAnchor

        box =
            Box
                (pos_ |> Pos.addX -cr |> Pos.addY -cr)
                (Size.square (2 * cr))
    in
    Svg.g
        (SvgAttrs.class "drag-control" :: attrs)
        [ Svg.circle
            [ SvgAttrs.cx <| Pos.toXString pos_
            , SvgAttrs.cy <| Pos.toYString pos_
            , SvgAttrs.r <| String.fromFloat <| cr
            ]
            []
        , SvgEx.text_ [] box input.txt
        , Svg.circle
            [ SvgAttrs.class "grip"
            , SvgAttrs.cx <| Pos.toXString pos_
            , SvgAttrs.cy <| Pos.toYString pos_
            , SvgAttrs.r <| String.fromFloat <| gr
            ]
            []
        ]


controlRadius : { r | active : Bool, unit : Float } -> Float
controlRadius { active, unit } =
    -- if active then
    --     unit / 1.2
    -- else
    --     unit / 3
    unit / 6


gripRadius : { r | active : Bool, unit : Float } -> Float
gripRadius { active, unit } =
    unit / 1.2



-- if active then
--     unit / 1.2
-- else
--     unit / 1.5
