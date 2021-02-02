module Block.Internal.View exposing (..)

import Block.Model exposing (..)
import Grid
import Pair
import Svg
import Svg.Attributes as SvgAttrs



-- RECT


type alias RectData =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , class : String
    }


toRectData : Data -> List RectData
toRectData bd =
    let
        ( headerWidth, headerHeight ) =
            if bd.headerOffset > 0 && bd.width > bd.headerOffset then
                ( bd.width - bd.headerOffset, 1 )

            else
                ( 0, 0 )

        header =
            { x = bd.x + bd.headerOffset
            , y = bd.y
            , width = headerWidth
            , height = headerHeight
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


viewRect : Scale -> RectData -> Svg.Svg msg
viewRect scale rd =
    Svg.g
        [ SvgAttrs.class rd.class ]
        (Grid.view
            { x = scale.unit * rd.x + scale.dx
            , y = scale.unit * rd.y + scale.dy
            , width = scale.unit * rd.width
            , height = scale.unit * rd.height
            , unit = scale.unit
            , isAlternateLine = \_ -> False
            }
        )



-- ADD/SUBTRACT CONTROL


viewAddControl : List (Svg.Attribute msg) -> Scale -> RectData -> Svg.Svg msg
viewAddControl attrs scale rd =
    let
        ( x, y ) =
            ( rd.x, rd.y )
                |> Pair.map ((*) scale.unit)
                |> Tuple.mapBoth ((+) scale.dx) ((+) scale.dy)
                |> Tuple.mapBoth ((+) (-1 * scale.unit)) ((+) (scale.unit // 2))

        ( dx, dy ) =
            ( 0, 0 )

        -- dragDelta bd
    in
    Svg.g
        attrs
        [ Svg.circle
            [ SvgAttrs.cx <| String.fromInt <| x + dx
            , SvgAttrs.cy <| String.fromInt <| y + dy
            , SvgAttrs.r <| String.fromInt <| scale.unit // 3
            , SvgAttrs.fill "rgb(85,209,229)"
            , SvgAttrs.stroke "rgb(85,209,229)"
            , SvgAttrs.cursor "w-resize"
            ]
            []
        ]



-- OFFSET CONTROL


viewOffsetControl : List (Svg.Attribute msg) -> Scale -> RectData -> Svg.Svg msg
viewOffsetControl attrs scale rd =
    let
        ( x, y ) =
            ( rd.x, rd.y )
                |> Pair.map ((*) scale.unit)
                |> Tuple.mapBoth ((+) scale.dx) ((+) scale.dy)
                |> Tuple.mapBoth ((+) (-1 * scale.unit)) ((+) (scale.unit // 2))

        ( dx, dy ) =
            ( 0, 0 )

        -- dragDelta bd
    in
    Svg.g
        attrs
        [ Svg.circle
            [ SvgAttrs.cx <| String.fromInt <| x + dx
            , SvgAttrs.cy <| String.fromInt <| y + dy
            , SvgAttrs.r <| String.fromInt <| scale.unit // 3
            , SvgAttrs.fill "rgb(85,209,229)"
            , SvgAttrs.stroke "rgb(85,209,229)"
            , SvgAttrs.cursor "w-resize"
            ]
            []
        ]



-- WIDTH CONTROL
