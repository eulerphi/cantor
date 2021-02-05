module Block.Internal.View.BodyModel exposing (..)

import Block.Internal.Types exposing (..)
import Delta
import Grid
import MaybeEx
import Pair
import Pos exposing (Pos)
import Size exposing (Size)
import ViewData exposing (ViewData)


type alias BodyModel =
    { top : Maybe ViewData
    , mid : ViewData
    , bot : Maybe ViewData
    }


size : BodyModel -> Size
size body =
    let
        getSizeOrNone =
            \mvd ->
                mvd
                    |> Maybe.map (\vd -> vd.size)
                    |> Maybe.withDefault Size.none
    in
    Size.add3
        (getSizeOrNone body.top)
        body.mid.size
        (getSizeOrNone body.bot)


forBlock : Grid.Data -> Block -> BodyModel
forBlock gd bd =
    let
        ( topWidth, topHeight ) =
            if bd.headerOffset > 0 && bd.width > bd.headerOffset then
                ( bd.width - bd.headerOffset, 1 )

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
    { top = Just top, mid = mid, bot = Just bot }
        |> scale gd
        |> addPos bd.pos
        |> filterEmpty


addPos : Pos -> BodyModel -> BodyModel
addPos pos body =
    { top = body.top |> Maybe.map (ViewData.addPos pos)
    , mid = body.mid |> ViewData.addPos pos
    , bot = body.bot |> Maybe.map (ViewData.addPos pos)
    }


filterEmpty : BodyModel -> BodyModel
filterEmpty body =
    { top = body.top |> MaybeEx.filter ViewData.hasSize
    , mid = body.mid
    , bot = body.bot |> MaybeEx.filter ViewData.hasSize
    }


scale : Grid.Data -> BodyModel -> BodyModel
scale gd body =
    let
        scaleFn =
            ViewData.scaleByInt gd.unit
    in
    { top = body.top |> Maybe.map scaleFn
    , mid = body.mid |> scaleFn
    , bot = body.bot |> Maybe.map scaleFn
    }


updateDragMoveDelta : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
updateDragMoveDelta oldDelta newDelta =
    Pair.add oldDelta newDelta