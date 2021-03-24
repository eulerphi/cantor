module Block.Internal.Section exposing
    ( Section
    , Section2
    , Sections
    , first
    , forBlock
    , forBlock2
    , forBlockFoo
    , last
    , midSection
    , remainderPos
    , titleText
    , toBox
    , toList
    , view
    )

import Block.Internal.Component exposing (Component(..))
import Block.Internal.Types exposing (..)
import Box exposing (Box)
import Grid exposing (Grid)
import List
import Maybe
import Maybe.Extra as MaybeEx
import OffsetAnchor exposing (OffsetAnchor)
import Pair
import Pos exposing (Pos)
import Size exposing (IntSize, Size)
import String
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


type alias Section =
    { pos : Pos
    , size : Size
    , sizeInUnits : IntSize
    , class : String
    , isMid : Bool
    , offset : Int
    , quantity : Int
    }


type alias Section2 =
    { pos : Pos
    , size : Size
    , children : Children
    , class : Class
    , modifier : Modifier
    , quantity : Int
    , unit : Float
    }


type alias Sections =
    { product : Maybe Section2
    , remainder : Maybe Section2
    }


type Class
    = Inactive
    | Product
    | Remainder


type Modifier
    = None
    | Adding
    | HideText
    | Removing


type Children
    = Children (List Section2)



-- PUBLIC API


toGrid : Section2 -> Grid
toGrid s =
    s |> Grid.forBox s.unit


toList : Sections -> List Section2
toList ss =
    [ ss.product, ss.remainder ] |> MaybeEx.values


view : List (Attribute msg) -> Section2 -> Svg msg
view attrs s =
    let
        attrs_ =
            (s |> classString |> SvgAttrs.class) :: attrs

        elems =
            [ s |> toGrid |> Grid.view []
            , s |> viewText
            ]

        kids =
            s |> children |> List.map (view [])
    in
    Svg.g attrs_ (elems ++ kids)


viewText : Section2 -> Svg msg
viewText s =
    let
        box =
            Size.square s.unit |> Box s.pos

        q =
            s.quantity |> String.fromInt

        txt =
            case s.modifier of
                None ->
                    Just q

                Adding ->
                    Just q

                HideText ->
                    Nothing

                Removing ->
                    Just q
    in
    case txt of
        Just str ->
            SvgEx.text_ [] box str

        Nothing ->
            Svg.g [] []


forSelectedBlock : Grid -> Block -> ( Section2, Section2 )
forSelectedBlock gd bd =
    let
        product =
            { pos = bd.pos
            , size = bd.product |> Size.toFloat |> Size.scale gd.unit
            , children = Children []
            , class = Product
            , modifier = None
            , quantity = bd.product |> Size.area
            , unit = gd.unit
            }

        remainder =
            { pos = bd.pos |> Pos.addY product.size.height
            , size = IntSize bd.remainder 1 |> Size.toFloat |> Size.scale gd.unit
            , children = Children []
            , class = Remainder
            , modifier = None
            , quantity = bd.remainder
            , unit = gd.unit
            }
    in
    ( product, remainder )


forIdleBlock : Grid -> Block -> ( Section2, Section2 )
forIdleBlock gd bd =
    let
        toIdleSection =
            \s ->
                { s | class = Inactive, quantity = bd.quantity }

        ( product, remainder ) =
            forSelectedBlock gd bd
                |> Pair.map toIdleSection

        remainderModifier =
            if hasProduct bd then
                HideText

            else
                None
    in
    ( product, { remainder | modifier = remainderModifier } )


hasProduct : Block -> Bool
hasProduct bd =
    bd.product.height > 0


hasRemainder : Block -> Bool
hasRemainder bd =
    bd.remainder > 0


forBlock2 : Grid -> Block -> Sections
forBlock2 gd bd =
    let
        ( product, remainder ) =
            case bd.state of
                Idle ->
                    forIdleBlock gd bd

                Dragging _ _ ->
                    forSelectedBlock gd bd

                Selected ->
                    forSelectedBlock gd bd
    in
    { product =
        if hasProduct bd then
            Just product

        else
            Nothing
    , remainder =
        if hasRemainder bd then
            Just remainder

        else
            Nothing
    }


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


children : Section2 -> List Section2
children s =
    case s.children of
        Children kids ->
            kids


classString : Section2 -> String
classString s =
    case s.class of
        Inactive ->
            "idle"

        Product ->
            case s.modifier of
                Adding ->
                    "product-add"

                Removing ->
                    "product-sub"

                _ ->
                    "product"

        Remainder ->
            "remainder"


hasSize : Section -> Bool
hasSize section =
    section |> Box.hasSize


productQuantity : Block -> Int
productQuantity bd =
    bd.product |> Size.area


scale : Float -> Section -> Section
scale value section =
    section |> Box.scale value
