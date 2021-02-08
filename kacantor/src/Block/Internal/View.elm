module Block.Internal.View exposing (view)

import Block.Internal.Component as Component exposing (Component)
import Block.Internal.Component.Body as BodyComponent
import Block.Internal.Component.Offset as OffsetControl
import Block.Internal.Component.Outline as OutlineComponent
import Block.Internal.Component.Quantity as QuantityComponent
import Block.Internal.Component.Ruler as Ruler
import Block.Internal.Component.Width as WidthComponent
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

        decorators =
            [ OutlineComponent.view [] vm
            , Ruler.view [] vm
            ]
                |> Maybe.Extra.values

        controls =
            [ WidthComponent.view (eventAttrsFn Component.Width) vm
            , OffsetControl.view (eventAttrsFn Component.Offset) vm
            , QuantityComponent.view (eventAttrsFn Component.Quantity) vm
            ]
                |> Maybe.Extra.values
    in
    Svg.g [ SvgAttrs.class "block" ] (body :: decorators ++ controls)


eventAttrs : (Msg -> msg) -> String -> Component -> List (Attribute msg)
eventAttrs envelop key part =
    let
        id =
            Id key part
    in
    Draggable.mouseTrigger id (envelop << DragMsg)
        :: Draggable.touchTriggers id (envelop << DragMsg)
