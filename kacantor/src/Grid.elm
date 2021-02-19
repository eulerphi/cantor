module Grid exposing
    ( Grid
    , Options
    , emptyGrid
    , forBox
    , forViewContext
    , view
    , viewWithOptions
    )

import Box exposing (Boxlike)
import Html exposing (..)
import Line exposing (Line)
import List
import Pair
import Pos exposing (Pos)
import Size exposing (Size)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx
import ViewContext exposing (ViewContext)



-- TYPES


type alias Grid =
    { pos : Pos
    , size : Size
    , unit : Float
    }


type alias Options =
    { isAlternateVerticalLine : IsAlternateLineFunction
    , isAlternateHorizontalLine : IsAlternateLineFunction
    }


type alias IsAlternateLineFunction =
    Int -> Bool



-- INIT


forBox : Float -> Boxlike r -> Grid
forBox unit box =
    Grid box.pos box.size unit


forViewContext : Float -> ViewContext msg -> Grid
forViewContext minUnits vc =
    let
        unit =
            ((vc.size |> Size.minDimension) / minUnits)
                |> round
                |> toFloat

        size =
            vc.size
                |> Size.inUnits unit
                |> Size.toFloat
                |> Size.scale unit

        pos =
            size
                |> Size.sub vc.size
                |> Size.div 2
                |> Size.toPair
                |> Pair.uncurry Pos
    in
    Grid pos size unit


emptyGrid : Grid
emptyGrid =
    Grid Pos.origin Size.none 0


emptyOptions : Options
emptyOptions =
    Options (\_ -> False) (\_ -> False)



-- VIEW


view : List (Attribute msg) -> Grid -> Svg msg
view attrs grid =
    viewWithOptions attrs emptyOptions grid


viewWithOptions : List (Attribute msg) -> Options -> Grid -> Svg msg
viewWithOptions attrs opts grid =
    let
        rect =
            SvgEx.rect [] grid

        vlineParams =
            { class = "vline"
            , isAlternate = opts.isAlternateVerticalLine
            , toLineFn = Line.addY grid.size.height
            , toPosFn = \x -> grid.pos |> Pos.addX x
            , unit = grid.unit
            }

        hlineParams =
            { class = "hline"
            , isAlternate = opts.isAlternateHorizontalLine
            , toLineFn = Line.addX grid.size.width
            , toPosFn = \y -> grid.pos |> Pos.addY y
            , unit = grid.unit
            }

        ( hlines, vlines ) =
            grid.size
                |> Size.inUnits grid.unit
                |> Size.toPair
                |> Pair.map (\d -> List.range 1 (d - 1))
                |> Tuple.mapBoth
                    (List.map (viewLine vlineParams))
                    (List.map (viewLine hlineParams))
    in
    Svg.g
        (SvgAttrs.class "grid" :: attrs)
        (rect :: hlines ++ vlines)



-- PRIVATE


viewLine :
    { class : String
    , isAlternate : IsAlternateLineFunction
    , toLineFn : Pos -> Line
    , toPosFn : Float -> Pos
    , unit : Float
    }
    -> Int
    -> Svg msg
viewLine { class, isAlternate, toLineFn, toPosFn, unit } idx =
    let
        class_ =
            if isAlternate idx then
                class ++ "-alternate"

            else
                class
    in
    idx
        |> toFloat
        |> (*) unit
        |> toPosFn
        |> toLineFn
        |> SvgEx.line [ SvgAttrs.class class_ ]
