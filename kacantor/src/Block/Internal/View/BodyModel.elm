module Block.Internal.View.BodyModel exposing (..)

import Block.Internal.Types exposing (..)
import Grid
import Maybe.Extra
import MaybeEx
import Pair
import Pos exposing (Pos)
import Size exposing (Size)
import ViewData exposing (ViewData)


type alias BodyModel =
    { top : Maybe ViewData
    , mid : ViewData
    , bot : Maybe ViewData
    , str1 : String
    , str2 : String
    }


forBlock : Grid.Data -> Block -> BodyModel
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
            { class = "body-top"
            , pos = Pos.fromInt ( bd.headerOffset, 0 )
            , size = Size.fromInt ( topWidth, topHeight )
            }

        mid =
            { class = "body-mid"
            , pos = Pos.fromInt ( 0, topHeight )
            , size = Size.fromInt ( midWidth, midHeight )
            }

        bot =
            { class = "body-bot"
            , pos = Pos.fromInt ( 0, topHeight + midHeight )
            , size = Size.fromInt ( botWidth, botHeight )
            }
    in
    { top = Just top
    , mid = mid
    , bot = Just bot
    , str1 = String.fromInt bd.quantity
    , str2 = ""
    }
        |> filterEmpty
        |> fillVals
        |> scale gd
        |> addPos bd.pos


first : BodyModel -> ViewData
first bm =
    bm.mid


size : BodyModel -> Size
size body =
    let
        getSizeOrNone =
            \mvd ->
                mvd
                    |> Maybe.map (\vd -> vd.size)
                    |> Maybe.withDefault Size.none

        s1 =
            getSizeOrNone body.top

        s2 =
            body.mid.size

        s3 =
            getSizeOrNone body.bot

        width =
            Size.maxWidth3 s1 s2 s3

        height =
            s1.height + s2.height + s3.height
    in
    Size.init ( width, height )


addPos : Pos -> BodyModel -> BodyModel
addPos pos body =
    { body
        | top = body.top |> Maybe.map (ViewData.addPos pos)
        , mid = body.mid |> ViewData.addPos pos
        , bot = body.bot |> Maybe.map (ViewData.addPos pos)
    }


fillVals : BodyModel -> BodyModel
fillVals body =
    let
        top =
            body.top |> Maybe.map (\vd -> String.fromFloat vd.size.width)

        mid =
            if body.mid.size.height > 0 then
                Just <| String.fromFloat <| body.mid.size.height * body.mid.size.width

            else
                Nothing

        bot =
            body.bot |> Maybe.map (\vd -> String.fromFloat vd.size.width)
    in
    { body | str2 = String.join " + " <| Maybe.Extra.values [ top, mid, bot ] }


filterEmpty : BodyModel -> BodyModel
filterEmpty body =
    { body
        | top = body.top |> MaybeEx.filter ViewData.hasSize
        , mid = body.mid
        , bot = body.bot |> MaybeEx.filter ViewData.hasSize
    }


scale : Grid.Data -> BodyModel -> BodyModel
scale gd body =
    let
        scaleFn =
            ViewData.scaleByInt gd.unit
    in
    { body
        | top = body.top |> Maybe.map scaleFn
        , mid = body.mid |> scaleFn
        , bot = body.bot |> Maybe.map scaleFn
    }


updateDragMoveDelta : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
updateDragMoveDelta oldDelta newDelta =
    Pair.add oldDelta newDelta
