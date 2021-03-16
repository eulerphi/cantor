module Block.Internal.Section exposing
    ( Section
    , first
    , forBlock
    , forBlockFoo
    , last
    , midSection
    , remainderPos
    , titleText
    , toBox
    )

import Block.Internal.Component exposing (Component(..))
import Block.Internal.Types exposing (..)
import Box exposing (Box)
import Grid exposing (Grid)
import Maybe
import Maybe.Extra as MaybeEx
import OffsetAnchor exposing (OffsetAnchor)
import Pair
import Pos exposing (Pos)
import Size exposing (IntSize, Size)


type alias Section =
    { pos : Pos
    , size : Size
    , sizeInUnits : IntSize
    , class : String
    , isMid : Bool
    , offset : Int
    , quantity : Int
    }



-- PUBLIC API


forBlock : Grid -> Block -> List Section
forBlock gd bd =
    forBlockInternal gd bd


forBlockFoo : Grid -> Block -> ( List Section, List Section )
forBlockFoo gd bd =
    case bd.state of
        Dragging ctx (QuantityDrag _) ->
            let
                ( newQuantity, oldQuantity ) =
                    ( bd.quantity, ctx.bd.quantity )

                delta =
                    newQuantity - oldQuantity

                sections =
                    { bd | quantity = min oldQuantity (oldQuantity + delta) }
                        |> forBlockInternal gd

                temps =
                    { bd
                        | quantity = abs delta
                        , pos =
                            sections
                                |> last
                                |> Maybe.map
                                    (\s ->
                                        if s.sizeInUnits.width == bd.width then
                                            s.pos
                                                |> Pos.addY s.size.height
                                                |> Pos.updateX bd.pos.x

                                        else
                                            s.pos |> Pos.updateX bd.pos.x
                                    )
                                |> Maybe.withDefault bd.pos
                        , headerOffset =
                            sections
                                |> last
                                |> Maybe.map
                                    (\s ->
                                        if s.sizeInUnits.width == bd.width then
                                            0

                                        else
                                            s.sizeInUnits.width + s.offset
                                    )
                                |> Maybe.withDefault bd.headerOffset
                    }
                        |> forBlockInternal gd
            in
            ( sections, temps )

        _ ->
            ( forBlockInternal gd bd, [] )


forBlockInternal : Grid -> Block -> List Section
forBlockInternal gd bd =
    let
        topSize =
            if bd.headerOffset > 0 then
                IntSize (min bd.quantity (bd.width - bd.headerOffset)) 1

            else
                Size.noneInt

        midSize =
            IntSize bd.width <| (bd.quantity - topSize.width) // bd.width

        botSize =
            IntSize
                (bd.quantity - topSize.width - (midSize.width * midSize.height))
                1

        top =
            { pos =
                Pos.fromInt ( bd.headerOffset, 0 )
            , size = topSize |> Size.toFloat
            , sizeInUnits = topSize
            , class = "body-top"
            , isMid = False
            , quantity = topSize |> Size.area
            , offset = bd.headerOffset
            }

        mid =
            { pos =
                Pos.fromInt ( 0, topSize.height )
            , size = midSize |> Size.toFloat
            , sizeInUnits = midSize
            , class = "body-mid"
            , isMid = True
            , quantity = midSize |> Size.area
            , offset = 0
            }

        bot =
            { pos =
                Pos.fromInt ( 0, topSize.height + midSize.height )
            , size = botSize |> Size.toFloat
            , sizeInUnits = botSize
            , class = "body-bot"
            , isMid = False
            , quantity = botSize |> Size.area
            , offset = 0
            }
    in
    [ top, mid, bot ]
        |> List.filter hasSize
        |> List.map (scale gd.unit)
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


midSection : List Section -> Maybe Section
midSection sections =
    sections
        |> List.filter (\s -> s.isMid)
        |> List.head


titleText : Section -> String
titleText section =
    if section.isMid then
        section.sizeInUnits
            |> Size.toPair
            |> Pair.map String.fromInt
            |> (\( w, h ) -> h ++ " x " ++ w)

    else
        section.quantity
            |> String.fromInt


toBox : Grid -> Block -> List Section -> Box
toBox gd bd sections =
    let
        width =
            bd.width |> toFloat |> (*) gd.unit

        height =
            sections
                |> last
                |> Maybe.map (\s -> s.pos.y + s.size.height - bd.pos.y)
                |> Maybe.withDefault 0
    in
    Box bd.pos (Size width height)


rectPos : OffsetAnchor -> List Section -> Pos
rectPos anchor sections =
    Pos.origin


remainderPos : OffsetAnchor -> List Section -> Pos
remainderPos anchor sections =
    last sections
        -- |> MaybeEx.orElse (midSection sections)
        |> Maybe.map Box.asBox
        |> Maybe.withDefault Box.none
        |> OffsetAnchor.toPos anchor



-- PRIVATE API


addPos : Pos -> Section -> Section
addPos pos section =
    { section | pos = Pos.add pos section.pos }


hasSize : Section -> Bool
hasSize section =
    section |> Box.hasSize


scale : Float -> Section -> Section
scale value section =
    section |> Box.scale value
