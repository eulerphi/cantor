module Block.Internal.Update exposing (update)

import Block.Internal.Component as Component
import Block.Internal.Component.Body as Body
import Block.Internal.Component.Offset as OffsetControl
import Block.Internal.Component.Quantity as QuantityControl
import Block.Internal.Component.Width as WidthControl
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model as ViewModel
import Delta exposing (Delta)
import DragState
import Draggable
import Draggable.Events
import Grid


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
            ( model |> Maybe.map (dragMove delta gd)
            , context
            , Cmd.none
            )

        EndDrag ->
            ( model |> Maybe.andThen (endDrag gd)
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


startDrag : Id -> Grid.Data -> Block -> Block
startDrag id gd bd =
    let
        vm =
            ViewModel.forBlock gd bd

        drag =
            case id.part of
                Component.Body ->
                    Body.dragStart vm bd

                Component.Offset ->
                    OffsetControl.startDrag vm bd

                Component.Quantity ->
                    QuantityControl.startDrag vm bd

                Component.Width ->
                    WidthControl.startDrag vm bd
    in
    { bd | state = Dragging id.part drag }


dragStart : Grid.Data -> Id -> Block -> Block
dragStart gd id bd =
    let
        vm =
            ViewModel.forBlock2 gd bd

        component_ =
            case id.part of
                Component.Body ->
                    DragBody <| Body.dragStart2 vm

                _ ->
                    DragBody <| Body.dragStart2 vm

        -- Component.Offset ->
        --     OffsetControl.startDrag vm bd
        -- Component.Quantity ->
        --     QuantityControl.startDrag vm bd
        -- Component.Width ->
        --     WidthControl.startDrag vm bd
    in
    { bd | state = Dragging2 <| DragData gd bd component_ }


dragMove : Delta -> Grid.Data -> Block -> Block
dragMove newDelta gd bd =
    case bd.state of
        Dragging component drag ->
            let
                drag_ =
                    DragState.add newDelta drag

                bd_ =
                    case component of
                        Component.Body ->
                            Body.dragMove drag_ gd bd

                        Component.Offset ->
                            OffsetControl.dragMove drag_ gd bd

                        Component.Quantity ->
                            QuantityControl.dragMove drag_ gd bd

                        Component.Width ->
                            WidthControl.dragMove drag_ gd bd
            in
            { bd_ | state = Dragging component drag_ }

        Dragging2 data ->
            let
                component_ =
                    case data.component of
                        DragBody state ->
                            DragBody (state |> Body.dragUpdate newDelta)

                        _ ->
                            data.component

                data_ =
                    { data | component = component_ }

                bd_ =
                    case data_.component of
                        DragBody state ->
                            state |> Body.dragMove2 data_.gd data_.bd

                        _ ->
                            data_.bd
            in
            { bd_ | state = Dragging2 data_ }

        _ ->
            bd


endDrag : Grid.Data -> Block -> Maybe Block
endDrag gd bd =
    case bd.state of
        Dragging component drag ->
            let
                bd_ =
                    case component of
                        Component.Body ->
                            Body.dragEnd drag gd bd

                        Component.Offset ->
                            OffsetControl.dragEnd drag gd bd

                        Component.Quantity ->
                            QuantityControl.dragEnd drag gd bd

                        Component.Width ->
                            WidthControl.dragEnd drag gd bd
            in
            bd_ |> Maybe.map (\b -> { b | state = Selected })

        Dragging2 data ->
            let
                bd_ =
                    case data.component of
                        DragBody state ->
                            Body.dragEnd2 data.gd data.bd state

                        _ ->
                            Just data.bd
            in
            bd_ |> Maybe.map (\b -> { b | state = Selected })

        _ ->
            Just bd
