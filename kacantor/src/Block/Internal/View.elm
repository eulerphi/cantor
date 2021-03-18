module Block.Internal.View exposing (view)

import Block.Internal.Component as Component exposing (Component)
import Block.Internal.Component.Body as Body
import Block.Internal.Component.Multiplicand as Multiplicand
import Block.Internal.Component.Multiplier as Multiplier
import Block.Internal.Component.Offset as OffsetControl
import Block.Internal.Component.Outline as OutlineComponent
import Block.Internal.Component.Remainder as Remainder
import Block.Internal.Component.Ruler as Ruler
import Block.Internal.Component.Width as WidthComponent
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel as ViewModel
import Draggable
import Grid exposing (Grid)
import Maybe.Extra
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs


view : Context msg -> Grid -> Block -> Svg msg
view context gd bd =
    let
        attrsFn =
            eventAttrs context.envelop bd.key

        vm =
            ViewModel.forBlock2 gd bd

        body =
            vm |> Body.view2 (attrsFn Component.Body)

        elements =
            [ OutlineComponent.view []
            , Multiplicand.view (attrsFn Component.Multiplicand)
            , Multiplier.view (attrsFn Component.Multiplier)
            , Remainder.view (attrsFn Component.Remainder)
            , WidthComponent.view (attrsFn Component.Width)
            ]
                |> List.map (\fn -> fn vm)
                |> Maybe.Extra.values

        -- body =
        --     { vm | sections = vm.tempBodySections }
        --         |> BodyComponent.view (attrsFn Component.Body)
        -- temps =
        --     { vm | sections = vm.tempChangeSections }
        --         |> BodyComponent.view [ SvgAttrs.class "temps" ]
        -- elements =
        --     [ OutlineComponent.view []
        --     , Multiplicand.view (attrsFn Component.Multiplicand)
        --     , Multiplier.view (attrsFn Component.Multiplier)
        --     , OffsetControl.view (attrsFn Component.Offset)
        --     , Remainder.view (attrsFn Component.Remainder)
        --     ]
        --         |> List.map (\fn -> fn vm)
        --         |> Maybe.Extra.values
    in
    Svg.g
        [ SvgAttrs.class "block" ]
        (body :: elements)


eventAttrs : (Msg -> msg) -> String -> Component -> List (Attribute msg)
eventAttrs envelop key part =
    let
        id =
            Id key part
    in
    Draggable.mouseTrigger id (envelop << DragMsg)
        :: Draggable.touchTriggers id (envelop << DragMsg)
