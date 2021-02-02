module Block.Internal.WidthControl exposing (..)

import Block.Internal.View exposing (RectData)
import Block.Model exposing (..)
import Grid
import Pair
import Svg
import Svg.Attributes as SvgAttrs



-- UPDATE


updateDragMoveDelta : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
updateDragMoveDelta oldDelta newDelta =
    Pair.addFirst oldDelta newDelta


updateWidth : Grid.Data -> Data -> Data
updateWidth gd bd =
    case bd.state of
        Dragging WidthControl delta ->
            bd

        _ ->
            bd



-- VIEW


view : List (Svg.Attribute msg) -> Scale -> Data -> RectData -> Svg.Svg msg
view attrs scale bd rd =
    let
        radius =
            case bd.state of
                Dragging WidthControl _ ->
                    scale.unit

                _ ->
                    round (toFloat scale.unit / 1.5)

        lineWidth =
            5

        ( x, y ) =
            ( rd.x, rd.y )
                |> Pair.map ((*) scale.unit)
                |> Tuple.mapBoth ((+) scale.dx) ((+) scale.dy)
                |> Tuple.mapFirst ((+) (scale.unit * rd.width))
                |> Tuple.mapFirst ((+) lineWidth)

        -- dragDelta bd
    in
    Svg.g
        (SvgAttrs.class "width-control" :: attrs)
        [ Svg.line
            [ SvgAttrs.x1 <| String.fromInt <| x
            , SvgAttrs.y1 <| String.fromInt <| y
            , SvgAttrs.x2 <| String.fromInt <| x
            , SvgAttrs.y2 <| String.fromInt <| y + 100
            , SvgAttrs.strokeWidth <| String.fromInt <| lineWidth
            ]
            []
        , Svg.circle
            [ SvgAttrs.cx <| String.fromInt <| x
            , SvgAttrs.cy <| String.fromInt <| y
            , SvgAttrs.r <| String.fromInt <| radius
            ]
            []
        ]
