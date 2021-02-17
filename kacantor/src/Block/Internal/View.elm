module Block.Internal.View exposing (view)

import Block.Internal.Component as Component exposing (Component)
import Block.Internal.Component.Body as BodyComponent
import Block.Internal.Component.Offset as OffsetControl
import Block.Internal.Component.Outline as OutlineComponent
import Block.Internal.Component.Quantity as QuantityComponent
import Block.Internal.Component.Ruler as Ruler
import Block.Internal.Component.Title as TitleComponent
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
        attrsFn =
            eventAttrs context.envelop bd.key

        vm =
            ViewModel.forBlock gd bd

        vm2 =
            ViewModel.forBlock2 gd bd

        body =
            vm2 |> BodyComponent.view (attrsFn Component.Body)

        elements =
            [ OutlineComponent.view []
            , Ruler.view []
            , TitleComponent.view
            , WidthComponent.view (attrsFn Component.Width)
            , OffsetControl.view (attrsFn Component.Offset)
            , QuantityComponent.view (attrsFn Component.Quantity)
            ]
                |> List.map (\fn -> fn vm)
                |> Maybe.Extra.values
    in
    Svg.g [ SvgAttrs.class "block" ] (body :: elements)


eventAttrs : (Msg -> msg) -> String -> Component -> List (Attribute msg)
eventAttrs envelop key part =
    let
        id =
            Id key part
    in
    Draggable.mouseTrigger id (envelop << DragMsg)
        :: Draggable.touchTriggers id (envelop << DragMsg)
