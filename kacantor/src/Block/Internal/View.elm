module Block.Internal.View exposing (view)

import Block.Internal.Component as Component exposing (Component)
import Block.Internal.Components.Body as BodyComponent
import Block.Internal.Components.Quantity as QuantityComponent
import Block.Internal.Components.Width as WidthComponent
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model as ViewModel
import Draggable
import Grid
import Maybe.Extra
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs


view : Context msg -> Grid.Data -> Block -> Svg msg
view context gd bd =
    let
        eventAttrsFn =
            eventAttrs context.envelop bd.key

        vm =
            ViewModel.forBlock gd bd

        body =
            BodyComponent.view (eventAttrsFn Component.Body) vm

        controls =
            [ WidthComponent.view (eventAttrsFn Component.Width) vm
            , QuantityComponent.view (eventAttrsFn Component.Quantity) vm
            ]
                |> Maybe.Extra.values
    in
    Svg.g [ SvgAttrs.class "block" ] (body :: controls)


eventAttrs : (Msg -> msg) -> String -> Component -> List (Attribute msg)
eventAttrs envelop key part =
    let
        id =
            Id key part
    in
    Draggable.mouseTrigger id (envelop << DragMsg)
        :: Draggable.touchTriggers id (envelop << DragMsg)
