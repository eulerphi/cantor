module Block exposing (Data, data, view, viewAll, withPos)

import Grid
import Html exposing (..)
import List
import Svg
import Svg.Attributes as SvgAttrs
import Tuple exposing (..)


type alias Data =
    { id : String
    , x : Int
    , y : Int
    , quantity : Int
    , headerOffset : Int
    , width : Int
    }


data : String -> Int -> Data
data id quantity =
    { id = id
    , x = 0
    , y = 0
    , quantity = quantity
    , headerOffset = 2
    , width = 10
    }


withPos : ( Int, Int ) -> Data -> Data
withPos pos bd =
    { bd | x = Tuple.first pos, y = Tuple.second pos }


view : Grid.Data -> Data -> Svg.Svg msg
view gd bd =
    let
        parts =
            toLogicalViewData bd
                |> List.map (toPhysicalViewData gd)
                |> List.map rect
                |> List.concat
    in
    Svg.g [ SvgAttrs.class "block" ] parts


viewAll : Grid.Data -> List Data -> List (Svg.Svg msg)
viewAll gd bds =
    bds |> List.map (view gd)



-- VIEW DATA


type alias ViewData =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
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
            , class = "block-header"
            }

        body =
            { x = bd.x
            , y = bd.y + header.height
            , width = bd.width
            , height = (bd.quantity - header.width) // bd.width
            , class = "block-body"
            }

        remainder =
            bd.quantity - header.width - (body.width * body.height)

        footer =
            { x = bd.x
            , y = bd.y + header.height + body.height
            , width = remainder
            , height = 1
            , class = "block-footer"
            }
    in
    [ header, body, footer ]
        |> List.filter (\vd -> vd.width > 0)


toPhysicalViewData : Grid.Data -> ViewData -> ViewData
toPhysicalViewData gd vd =
    { x = gd.x + (gd.offset * vd.x)
    , y = gd.y + (vd.y * gd.offset)
    , width = vd.width * gd.offset
    , height = vd.height * gd.offset
    , class = vd.class
    }



-- RECT


rect : ViewData -> List (Svg.Svg msg)
rect vd =
    [ Svg.rect
        [ SvgAttrs.x <| String.fromInt <| vd.x
        , SvgAttrs.y <| String.fromInt <| vd.y
        , SvgAttrs.width <| String.fromInt <| vd.width
        , SvgAttrs.height <| String.fromInt <| vd.height
        , SvgAttrs.class vd.class
        , SvgAttrs.fill "black"
        ]
        []
    ]
