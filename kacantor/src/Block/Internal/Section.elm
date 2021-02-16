module Block.Internal.Section exposing (Section, first, forBlock, last, toBox)

import Block.Internal.Types exposing (..)
import Box exposing (Box)
import Grid
import Pos exposing (Pos)
import Size exposing (Size)


type alias Section =
    { box : Box
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
            { box =
                Box.fromInt
                    { x = bd.headerOffset
                    , y = 0
                    , width = topWidth
                    , height = topHeight
                    }
            , class = "body-top"
            , quantity = topWidth
            }

        mid =
            { box =
                Box.fromInt
                    { x = 0
                    , y = topHeight
                    , width = midWidth
                    , height = midHeight
                    }
            , class = "body-mid"
            , quantity = midWidth * midHeight
            }

        bot =
            { box =
                Box.fromInt
                    { x = 0
                    , y = topHeight + midHeight
                    , width = botWidth
                    , height = botHeight
                    }
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
                |> Maybe.map (\s -> s.box.pos)
                |> Maybe.withDefault bd.pos

        size =
            sections
                |> List.map (\s -> s.box.size)
                |> List.foldl combineSize Size.none
    in
    Box pos size



-- PRIVATE API


addPos : Pos -> Section -> Section
addPos pos section =
    { section | box = Box.addPos pos section.box }


combineSize : Size -> Size -> Size
combineSize s1 s2 =
    Size (max s1.width s2.width) (s1.height + s2.height)


hasSize : Section -> Bool
hasSize section =
    section.box |> Box.hasSize


scale : Float -> Section -> Section
scale value section =
    { section | box = Box.scale value section.box }
