module Grid exposing
    ( Data
    , Grid
    , calculateUnit
    , centeredParams
    , emptyParams
    , forBox
    , toGrid
    , view
    )

import Box exposing (Boxlike)
import Html exposing (..)
import List
import Pos exposing (Pos)
import Size exposing (Size)
import Svg
import Svg.Attributes as SvgAttrs


type alias Grid =
    { pos : Pos
    , size : Size
    , unit : Float
    }


type alias Data =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , unit : Int
    , isAlternateLine : Int -> Bool
    }


forBox : Float -> Boxlike r -> Data
forBox unit box =
    { x = round box.pos.x
    , y = round box.pos.y
    , width = round box.size.width
    , height = round box.size.height
    , unit = round unit
    , isAlternateLine = \_ -> False
    }


toGrid : Data -> Grid
toGrid gd =
    { pos = Pos.fromInt ( gd.x, gd.y )
    , size = Size.fromInt ( gd.width, gd.height )
    , unit = toFloat gd.unit
    }


emptyParams : Data
emptyParams =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    , unit = 0
    , isAlternateLine = \_ -> False
    }


centeredParams : ( Int, Int ) -> Int -> Data
centeredParams wh unit =
    let
        svgWidth =
            Tuple.first wh

        svgHeight =
            Tuple.second wh

        gridWidth =
            (svgWidth // unit) * unit

        gridHeight =
            (svgHeight // unit) * unit

        x =
            (svgWidth - gridWidth) // 2

        y =
            (svgHeight - gridHeight) // 2
    in
    { x = x
    , y = y
    , width = gridWidth
    , height = gridHeight
    , unit = unit
    , isAlternateLine = \_ -> False
    }


calculateUnit : ( Int, Int ) -> Int -> Int
calculateUnit wh minUnits =
    min (Tuple.first wh) (Tuple.second wh) // minUnits


view : List (Svg.Attribute msg) -> Data -> Svg.Svg msg
view attrs params =
    let
        rect =
            Svg.rect
                [ SvgAttrs.x <| String.fromInt <| params.x
                , SvgAttrs.y <| String.fromInt <| params.y
                , SvgAttrs.width <| String.fromInt <| params.width
                , SvgAttrs.height <| String.fromInt <| params.height
                , SvgAttrs.class "grid-rect"
                ]
                []

        horizontalLines =
            List.range 1 (params.height // params.unit - 1)
                |> List.map (hline params)

        verticalLines =
            List.range 1 (params.width // params.unit - 1)
                |> List.map (vline params)
    in
    Svg.g
        attrs
        (rect :: horizontalLines ++ verticalLines)


lineClass : Bool -> String -> String
lineClass isAlternateLine baseClass =
    if isAlternateLine then
        baseClass ++ "-alternate"

    else
        baseClass


vline : Data -> Int -> Svg.Svg msg
vline p index =
    let
        offset =
            index * p.unit

        p1 =
            ( p.x + offset, p.y )

        p2 =
            ( p.x + offset, p.y + p.height )

        class =
            lineClass (p.isAlternateLine index) "grid-vline"
    in
    line p1 p2 class


hline : Data -> Int -> Svg.Svg msg
hline p index =
    let
        offset =
            index * p.unit

        p1 =
            ( p.x, p.y + offset )

        p2 =
            ( p.x + p.width, p.y + offset )

        class =
            lineClass (p.isAlternateLine index) "grid-hline"
    in
    line p1 p2 class


line : ( Int, Int ) -> ( Int, Int ) -> String -> Svg.Svg msg
line p1 p2 class =
    Svg.line
        [ SvgAttrs.x1 <| String.fromInt <| Tuple.first p1
        , SvgAttrs.y1 <| String.fromInt <| Tuple.second p1
        , SvgAttrs.x2 <| String.fromInt <| Tuple.first p2
        , SvgAttrs.y2 <| String.fromInt <| Tuple.second p2
        , SvgAttrs.class class
        ]
        []
