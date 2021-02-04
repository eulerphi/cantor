module Block.Internal.View exposing (view)

import Block.Internal.Controls.Body as BodyControl
import Block.Internal.Controls.Quantity as QuantityControl
import Block.Internal.Controls.Width as WidthControl
import Block.Internal.ViewModel as ViewModel exposing (ViewModel)
import Block.Internal.ViewModel.Body as Body exposing (BodyViewModel, updateDragMoveDelta)
import Block.Model as Model exposing (..)
import Draggable
import Draggable.Events
import Grid
import Maybe.Extra
import MaybeEx
import Pair
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import ViewData exposing (ViewData)



-- RECT


view : Context msg -> Grid.Data -> Data -> Svg.Svg msg
view context gd bd =
    let
        eventAttrsFn =
            eventAttrs context.envelop bd.key

        vm =
            ViewModel.forBlock gd bd

        elements =
            viewInternal eventAttrsFn vm
    in
    Svg.g [ SvgAttrs.class "block" ] elements


viewInternal :
    (Model.Part -> List (Attribute msg))
    -> ViewModel
    -> List (Svg msg)
viewInternal eventAttrsFn vm =
    let
        body =
            BodyControl.view (eventAttrsFn Model.Body) vm

        controls =
            [ WidthControl.view (eventAttrsFn Model.WidthControl) vm
            , QuantityControl.view (eventAttrsFn Model.QuantityControl) vm
            ]
                |> Maybe.Extra.values
    in
    body :: controls


eventAttrs : (Msg -> msg) -> String -> Part -> List (Svg.Attribute msg)
eventAttrs envelop key part =
    let
        id =
            Id key part
    in
    Draggable.mouseTrigger id (envelop << DragMsg)
        :: Draggable.touchTriggers id (envelop << DragMsg)


viewBody : List (Attribute msg) -> ViewModel -> Svg msg
viewBody attrs vm =
    let
        viewFn =
            \mvd -> mvd |> MaybeEx.toMappedList (viewRect vm)

        optional =
            [ vm.body.top, vm.body.bot ]
                |> List.map viewFn
                |> List.concat

        elements =
            viewRect vm vm.body.mid :: optional
    in
    Svg.g
        (SvgAttrs.class "block-body" :: attrs)
        elements


type alias RectData =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , class : String
    }



-- toRectData : Data -> List RectData
-- toRectData bd =
--     let
--         ( headerWidth, headerHeight ) =
--             if bd.headerOffset > 0 && bd.width > bd.headerOffset then
--                 ( bd.width - bd.headerOffset, 1 )
--             else
--                 ( 0, 0 )
--         header =
--             { x = bd.x + bd.headerOffset
--             , y = bd.y
--             , width = headerWidth
--             , height = headerHeight
--             , class = "block-header"
--             }
--         body =
--             { x = bd.x
--             , y = bd.y + header.height
--             , width = bd.width
--             , height = (bd.quantity - header.width) // bd.width
--             , class = "block-body"
--             }
--         remainder =
--             bd.quantity - header.width - (body.width * body.height)
--         footer =
--             { x = bd.x
--             , y = bd.y + header.height + body.height
--             , width = remainder
--             , height = 1
--             , class = "block-footer"
--             }
--     in
--     [ header, body, footer ]
--         |> List.filter (\vd -> vd.width > 0)


viewRect : ViewModel -> ViewData -> Svg.Svg msg
viewRect vm vd =
    let
        vd_ =
            vd |> ViewData.addPos vm.block
    in
    Svg.g
        [ SvgAttrs.class vd.class ]
        (Grid.view (Grid.forViewData vm.grid.unit vd_))



-- ADD/SUBTRACT CONTROL
-- viewAddControl : List (Svg.Attribute msg) -> Scale -> RectData -> Svg.Svg msg
-- viewAddControl attrs scale rd =
--     let
--         ( x, y ) =
--             ( rd.x, rd.y )
--                 |> Pair.map ((*) scale.unit)
--                 |> Tuple.mapBoth ((+) scale.dx) ((+) scale.dy)
--                 |> Tuple.mapBoth ((+) (-1 * scale.unit)) ((+) (scale.unit // 2))
--         ( dx, dy ) =
--             ( 0, 0 )
--         -- dragDelta bd
--     in
--     Svg.g
--         attrs
--         [ Svg.circle
--             [ SvgAttrs.cx <| String.fromInt <| x + dx
--             , SvgAttrs.cy <| String.fromInt <| y + dy
--             , SvgAttrs.r <| String.fromInt <| scale.unit // 3
--             , SvgAttrs.fill "rgb(85,209,229)"
--             , SvgAttrs.stroke "rgb(85,209,229)"
--             , SvgAttrs.cursor "w-resize"
--             ]
--             []
--         ]
-- -- OFFSET CONTROL
-- viewOffsetControl : List (Svg.Attribute msg) -> Scale -> RectData -> Svg.Svg msg
-- viewOffsetControl attrs scale rd =
--     let
--         ( x, y ) =
--             ( rd.x, rd.y )
--                 |> Pair.map ((*) scale.unit)
--                 |> Tuple.mapBoth ((+) scale.dx) ((+) scale.dy)
--                 |> Tuple.mapBoth ((+) (-1 * scale.unit)) ((+) (scale.unit // 2))
--         ( dx, dy ) =
--             ( 0, 0 )
--         -- dragDelta bd
--     in
--     Svg.g
--         attrs
--         [ Svg.circle
--             [ SvgAttrs.cx <| String.fromInt <| x + dx
--             , SvgAttrs.cy <| String.fromInt <| y + dy
--             , SvgAttrs.r <| String.fromInt <| scale.unit // 3
--             , SvgAttrs.fill "rgb(85,209,229)"
--             , SvgAttrs.stroke "rgb(85,209,229)"
--             , SvgAttrs.cursor "w-resize"
--             ]
--             []
--         ]
-- -- WIDTH CONTROL
