module Block exposing (Data, Group, data, endDrag, onDrag, startDrag, view, viewAll, withPos)

import Extra
import Grid
import Html exposing (..)
import List
import Pair
import Svg
import Svg.Attributes as SvgAttrs
import Tuple exposing (..)


type alias Data =
    { id : String
    , drag : Maybe ( Int, Int )
    , x : Int
    , y : Int
    , quantity : Int
    , headerOffset : Int
    , width : Int
    }


type alias Group =
    { idle : List Data
    , drag : Maybe Data
    }


data : String -> Int -> Data
data id quantity =
    { id = id
    , drag = Nothing
    , x = 0
    , y = 0
    , quantity = quantity
    , headerOffset = 0
    , width = min quantity 10
    }


endDrag : Grid.Data -> Data -> Data
endDrag gd bd =
    let
        ( dx, dy ) =
            bd.drag
                |> Maybe.withDefault ( 0, 0 )
                |> Pair.map (Extra.roundBy gd.unit)
                |> Pair.map (\v -> v // gd.unit)
    in
    { bd | x = bd.x + dx, y = bd.y + dy, drag = Nothing }


startDrag : Data -> Data
startDrag bd =
    { bd | drag = Just ( 0, 0 ) }


onDrag : ( Int, Int ) -> Data -> Data
onDrag delta bd =
    let
        drag =
            bd.drag |> Maybe.map (Pair.add delta)
    in
    { bd | drag = drag }


withPos : ( Int, Int ) -> Data -> Data
withPos pos bd =
    { bd | x = Tuple.first pos, y = Tuple.second pos }


view : List (Svg.Attribute msg) -> Grid.Data -> Data -> Svg.Svg msg
view attrs gd bd =
    let
        parts =
            toLogicalViewData bd
                |> List.map (toPhysicalViewData gd)
                |> List.map (rect gd)
    in
    Svg.g
        (SvgAttrs.class "block" :: attrs)
        parts


viewAll : List (Svg.Attribute msg) -> Grid.Data -> List Data -> List (Svg.Svg msg)
viewAll attrs gd bds =
    bds |> List.map (view attrs gd)



-- VIEW DATA


type alias ViewData =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , drag : Maybe ( Int, Int )
    , class : String
    }


toLogicalViewData : Data -> List ViewData
toLogicalViewData bd =
    let
        headerWidth =
            if bd.headerOffset > 0 && bd.width > bd.headerOffset then
                bd.width - bd.headerOffset

            else
                0

        header =
            { x = bd.x + bd.headerOffset
            , y = bd.y
            , width = headerWidth
            , height = 1
            , drag = bd.drag
            , class = "block-header"
            }

        body =
            { x = bd.x
            , y = bd.y + header.height
            , width = bd.width
            , height = (bd.quantity - header.width) // bd.width
            , drag = bd.drag
            , class = "block-body"
            }

        remainder =
            bd.quantity - header.width - (body.width * body.height)

        footer =
            { x = bd.x
            , y = bd.y + header.height + body.height
            , width = remainder
            , height = 1
            , drag = bd.drag
            , class = "block-footer"
            }
    in
    [ header, body, footer ]
        |> List.filter (\vd -> vd.width > 0)


toPhysicalViewData : Grid.Data -> ViewData -> ViewData
toPhysicalViewData gd vd =
    let
        ( dx, dy ) =
            Maybe.withDefault ( 0, 0 ) vd.drag
    in
    { x = gd.x + (gd.unit * vd.x + dx)
    , y = gd.y + (vd.y * gd.unit + dy)
    , width = vd.width * gd.unit
    , height = vd.height * gd.unit
    , drag = Nothing
    , class = vd.class
    }



-- RECT


rect : Grid.Data -> ViewData -> Svg.Svg msg
rect gd vd =
    Svg.g
        [ SvgAttrs.class vd.class ]
        (Grid.view
            { x = vd.x
            , y = vd.y
            , width = vd.width
            , height = vd.height
            , unit = gd.unit
            , isAlternateLine = \_ -> False
            }
        )
