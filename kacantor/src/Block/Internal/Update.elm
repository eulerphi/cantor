module Block.Internal.Update exposing (clearSelection, update)

import Block.Internal.Component as Component
import Block.Internal.Component.Body as Body
import Block.Internal.Component.Offset as OffsetControl
import Block.Internal.Component.Quantity as QuantityControl
import Block.Internal.Component.Width as WidthControl
import Block.Internal.Types exposing (..)
import Block.Internal.ViewModel as ViewModel
import Delta exposing (Delta)
import Draggable
import Draggable.Events
import Grid exposing (Grid)
import Pair


clearSelection : Block -> Block
clearSelection bd =
    let
        state_ =
            case bd.state of
                Dragging _ _ ->
                    Selected

                _ ->
                    Idle
    in
    { bd | state = state_ }


update :
    Context msg
    -> Grid
    -> Msg
    -> Maybe Block
    -> ( Maybe Block, Context msg, Cmd msg )
update context gd msg bd =
    case msg of
        DragMsg subMsg ->
            let
                ( context_, cmd_ ) =
                    Draggable.update
                        (dragConfig context.envelop)
                        subMsg
                        context
            in
            ( bd, context_, cmd_ )

        StartDrag id ->
            ( bd |> Maybe.map (dragStart gd id)
            , context
            , Cmd.none
            )

        DragMove delta ->
            ( bd |> Maybe.map (dragMove delta)
            , context
            , Cmd.none
            )

        EndDrag ->
            ( bd |> Maybe.andThen endDrag
            , context
            , Cmd.none
            )

        Select _ ->
            ( bd |> Maybe.map (\m -> { m | state = Selected })
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


dragStart : Grid -> Id -> Block -> Block
dragStart gd id bd =
    case bd.state of
        Dragging _ _ ->
            bd

        _ ->
            let
                ctx =
                    DragContext gd bd

                vm =
                    ViewModel.forBlock gd bd

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
            component_
                |> Maybe.map (\c -> { bd | state = Dragging ctx c })
                |> Maybe.withDefault bd


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
                                |> Pair.fork DragOffset (OffsetControl.dragMove ctx)

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
                            OffsetControl.dragEnd ctx state

                        DragQuantity state ->
                            QuantityControl.dragEnd ctx state

                        DragWidth state ->
                            WidthControl.dragEnd ctx state
            in
            bd_ |> Maybe.map (\b -> { b | state = Selected })

        -- bd_
        _ ->
            Just bd
