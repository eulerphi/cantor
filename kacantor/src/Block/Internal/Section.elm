module Block.Internal.Section exposing (Section, first, forBlock, last, toBox)

import Block.Internal.Types exposing (..)
import Box exposing (Box)
import Grid
import Pos exposing (Pos)
import Size exposing (Size)


type alias Section =
    { pos : Pos
    , size : Size
    , class : String
    , quantity : Int
    }



-- PUBLIC API


forBlock : Grid.Data -> Block -> List Section
forBlock gd bd =
    let
        ( topWidth, topHeight ) =
            if bd.headerOffset > 0 then
                ( min bd.quantity (bd.width - bd.headerOffset), 1 )

            else
                ( 0, 0 )

        ( midWidth, midHeight ) =
            ( bd.width
            , (bd.quantity - topWidth) // bd.width
            )

        ( botWidth, botHeight ) =
            ( bd.quantity - topWidth - (midWidth * midHeight)
            , 1
            )

        top =
            { pos =
                Pos.fromInt ( bd.headerOffset, 0 )
            , size =
                Size.fromInt ( topWidth, topHeight )
            , class = "body-top"
            , quantity = topWidth
            }

        mid =
            { pos =
                Pos.fromInt ( 0, topHeight )
            , size =
                Size.fromInt ( midWidth, midHeight )
            , class = "body-mid"
            , quantity = midWidth * midHeight
            }

        bot =
            { pos =
                Pos.fromInt ( 0, topHeight + midHeight )
            , size =
                Size.fromInt ( botWidth, botHeight )
            , class = "body-bot"
            , quantity = botWidth
            }
    in
    [ top, mid, bot ]
        |> List.filter hasSize
        |> List.map (scale <| toFloat gd.unit)
        |> List.map (addPos bd.pos)


first : List Section -> Maybe Section
first sections =
    List.head sections


last : List Section -> Maybe Section
last sections =
    case sections of
        [] ->
            Nothing

        x :: [] ->
            Just x

        _ :: xs ->
            last xs


toBox : Block -> List Section -> Box
toBox bd sections =
    let
        pos =
            first sections
                |> Maybe.map .pos
                |> Maybe.withDefault bd.pos

        size =
            sections
                |> List.map .size
                |> List.foldl combineSize Size.none
    in
    Box pos size



-- PRIVATE API


addPos : Pos -> Section -> Section
addPos pos section =
    { section | pos = Pos.add pos section.pos }


combineSize : Size -> Size -> Size
combineSize s1 s2 =
    Size (max s1.width s2.width) (s1.height + s2.height)


hasSize : Section -> Bool
hasSize section =
    section |> Box.hasSize


scale : Float -> Section -> Section
scale value section =
    section |> Box.scale value
