module Grid exposing (Data, centeredParams, emptyParams, getOffset, view)

import Html exposing (..)
import List
import Svg
import Svg.Attributes as SvgAttrs


type alias Data =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , offset : Int
    , alternateCount : Int
    }


emptyParams : Data
emptyParams =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    , offset = 0
    , alternateCount = 0
    }


centeredParams : ( Int, Int ) -> Int -> Data
centeredParams wh offset =
    let
        svgWidth =
            Tuple.first wh

        svgHeight =
            Tuple.second wh

        gridWidth =
            (svgWidth // offset) * offset

        gridHeight =
            (svgHeight // offset) * offset

        x =
            (svgWidth - gridWidth) // 2

        y =
            (svgHeight - gridHeight) // 2
    in
    { x = x
    , y = y
    , width = gridWidth
    , height = gridHeight
    , offset = offset
    , alternateCount = 0
    }


getOffset : ( Int, Int ) -> Int -> Int
getOffset wh minUnits =
    min (Tuple.first wh) (Tuple.second wh) // minUnits


view : Data -> List (Html msg)
view params =
    let
        horizontalLines =
            List.range 0 (params.height // params.offset)
                |> List.map (hline params)

        verticalLines =
            List.range 0 (params.width // params.offset)
                |> List.map (vline params)
    in
    horizontalLines ++ verticalLines


lineClass : Int -> Int -> String -> String
lineClass index alternateCount baseClass =
    if index /= 0 && modBy alternateCount index == 0 then
        baseClass ++ "-alternate"

    else
        baseClass


vline : Data -> Int -> Html msg
vline p index =
    let
        offset =
            index * p.offset

        p1 =
            ( p.x + offset, p.y )

        p2 =
            ( p.x + offset, p.y + p.height )

        class =
            lineClass index p.alternateCount "vline"
    in
    line p1 p2 class


hline : Data -> Int -> Html msg
hline p index =
    let
        offset =
            index * p.offset

        p1 =
            ( p.x, p.y + offset )

        p2 =
            ( p.x + p.width, p.y + offset )

        class =
            lineClass index p.alternateCount "hline"
    in
    line p1 p2 class


line : ( Int, Int ) -> ( Int, Int ) -> String -> Html msg
line p1 p2 class =
    Svg.line
        [ SvgAttrs.x1 <| String.fromInt <| Tuple.first p1
        , SvgAttrs.y1 <| String.fromInt <| Tuple.second p1
        , SvgAttrs.x2 <| String.fromInt <| Tuple.first p2
        , SvgAttrs.y2 <| String.fromInt <| Tuple.second p2
        , SvgAttrs.class class
        , SvgAttrs.stroke "black"
        ]
        []
