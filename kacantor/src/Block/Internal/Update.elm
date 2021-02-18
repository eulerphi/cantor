module Block.Internal.Update exposing (update)

import Block.Internal.Component as Component
import Block.Internal.Component.Body as Body
import Block.Internal.Component.Offset as OffsetControl
import Block.Internal.Component.Quantity as QuantityControl
import Block.Internal.Component.Width as WidthControl
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model as ViewModel
import Delta exposing (Delta)
import Draggable
import Draggable.Events
import Grid
import Pair


update :
    Context msg
    -> Grid.Data
    -> Msg
    -> Maybe Block
    -> ( Maybe Block, Context msg, Cmd msg )
update context gd msg model =
    case msg of
        DragMsg subMsg ->
            let
                ( context_, cmd_ ) =
                    Draggable.update
                        (dragConfig context.envelop)
                        subMsg
                        context
            in
            ( model, context_, cmd_ )

        StartDrag id ->
            ( model |> Maybe.map (dragStart gd id)
            , context
            , Cmd.none
            )

        DragMove delta ->
            ( model |> Maybe.map (dragMove delta)
            , context
            , Cmd.none
            )

        EndDrag ->
            ( model |> Maybe.andThen endDrag
            , context
            , Cmd.none
            )

        Select _ ->
            ( model |> Maybe.map (\m -> { m | state = Selected })
            , context
            , Cmd.none
            )


dragConfig : (Msg -> msg) -> Draggable.Config Id msg
dragConfig envelop =
    Draggable.customConfig
        [ Draggable.Events.onDragStart <| envelop << StartDrag
        , Draggable.Events.onDragBy <| envelop << DragMove << Delta.init
        , Draggable.Events.onDragEnd <| envelop EndDrag
        , Draggable.Events.onClick <| envelop << Select
        ]


dragStart : Grid.Data -> Id -> Block -> Block
dragStart gd id bd =
    let
        ctx =
            DragContext (Grid.toGrid gd) bd

        grid =
            Grid.toGrid gd

        vm =
            ViewModel.forBlock2 grid bd

        component_ =
            case id.part of
                Component.Body ->
                    vm |> Body.dragStart |> Maybe.map DragBody

                Component.Offset ->
                    vm |> OffsetControl.dragStart |> Maybe.map DragOffset

                Component.Quantity ->
                    vm |> QuantityControl.dragStart |> Maybe.map DragQuantity

                Component.Width ->
                    vm |> WidthControl.dragStart |> Maybe.map DragWidth
    in
    case component_ of
        Just c ->
            { bd | state = Dragging ctx c }

        Nothing ->
            bd


dragMove : Delta -> Block -> Block
dragMove newDelta bd =
    case bd.state of
        Dragging ctx component ->
            let
                ( component_, bd_ ) =
                    case component of
                        DragBody state ->
                            state
                                |> Body.dragUpdate newDelta
                                |> Pair.fork DragBody (Body.dragMove ctx)

                        DragOffset state ->
                            state
                                |> OffsetControl.dragUpdate newDelta
                                |> Pair.fork DragOffset (OffsetControl.dragMove2 ctx)

                        DragQuantity state ->
                            state
                                |> QuantityControl.dragUpdate newDelta
                                |> Pair.fork DragQuantity (QuantityControl.dragMove ctx)

                        DragWidth state ->
                            state
                                |> WidthControl.dragUpdate newDelta
                                |> Pair.fork DragWidth (WidthControl.dragMove ctx)
            in
            { bd_ | state = Dragging ctx component_ }

        _ ->
            bd


endDrag : Block -> Maybe Block
endDrag bd =
    case bd.state of
        Dragging ctx component ->
            let
                bd_ =
                    case component of
                        DragBody state ->
                            Body.dragEnd ctx state

                        DragOffset state ->
                            OffsetControl.dragEnd2 ctx state

                        DragQuantity state ->
                            QuantityControl.dragEnd ctx state

                        DragWidth state ->
                            WidthControl.dragEnd ctx state
            in
            bd_ |> Maybe.map (\b -> { b | state = Selected })

        _ ->
            Just bd
