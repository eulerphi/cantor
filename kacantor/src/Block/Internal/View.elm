module Block.Internal.View exposing (view)

import Block.Internal.Component as Component exposing (Component)
import Block.Internal.Component.Body as BodyComponent
import Block.Internal.Component.Multiplicand as Multiplicand
import Block.Internal.Component.Multiplier as Multiple
import Block.Internal.Component.Offset as OffsetControl
import Block.Internal.Component.Outline as OutlineComponent
import Block.Internal.Component.Quantity as QuantityComponent
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
            ViewModel.forBlock gd bd

        body =
            { vm | sections = vm.tempBodySections }
                |> BodyComponent.view (attrsFn Component.Body)

        temps =
            { vm | sections = vm.tempChangeSections }
                |> BodyComponent.view [ SvgAttrs.class "temps" ]

        elements =
            [ OutlineComponent.view []
            , Multiple.view []
            , Multiplicand.view []

            -- , Ruler.view []
            , OffsetControl.view (attrsFn Component.Offset)
            , QuantityComponent.view (attrsFn Component.Quantity)

            -- , WidthComponent.view (attrsFn Component.Width)
            ]
                |> List.map (\fn -> fn vm)
                |> Maybe.Extra.values
    in
    Svg.g [ SvgAttrs.class "block" ] ([ body, temps ] ++ elements)


eventAttrs : (Msg -> msg) -> String -> Component -> List (Attribute msg)
eventAttrs envelop key part =
    let
        id =
            Id key part
    in
    Draggable.mouseTrigger id (envelop << DragMsg)
        :: Draggable.touchTriggers id (envelop << DragMsg)
