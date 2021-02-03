module Block.Internal.Body exposing (..)

import Block.Model exposing (..)
import Grid
import MaybeEx
import Pair
import Pos
import Size exposing (Size)
import Svg
import Svg.Attributes as SvgAttrs
import ViewData exposing (ViewData)


type alias Body =
    { top : Maybe ViewData
    , mid : ViewData
    , bot : Maybe ViewData
    }


size : Body -> Size
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


toBody : Grid.Data -> Block.Model.Data -> Body
toBody gd bd =
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
        |> filterEmpty


filterEmpty : Body -> Body
filterEmpty body =
    { top = body.top |> MaybeEx.filter ViewData.hasSize
    , mid = body.mid
    , bot = body.bot |> MaybeEx.filter ViewData.hasSize
    }


scale : Grid.Data -> Body -> Body
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
