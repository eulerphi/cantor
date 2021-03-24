module Block.Internal.Update exposing (clearSelection, update)

import Block.Internal.Component as Component exposing (Component(..))
import Block.Internal.Component.Body as Body
import Block.Internal.Component.Multiplicand as MultiplicandControl
import Block.Internal.Component.Multiplier as MultiplierControl
import Block.Internal.Component.Offset as OffsetControl
import Block.Internal.Component.Quantity as QuantityControl
import Block.Internal.Component.Remainder as Remainder
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
                -- Dragging _ _ ->
                --     Selected
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
    case bd.drag of
        Just _ ->
            bd

        Nothing ->
            let
                ctx =
                    DragContext gd bd

                vm =
                    ViewModel.forBlock2 gd bd

                drag_ =
                    case id.part of
                        Component.Body ->
                            vm |> Body.dragStart |> Maybe.map BodyDrag

                        _ ->
                            Nothing
            in
            drag_
                |> Maybe.map (\d -> { bd | drag = Just ( DragCtx ctx, d ) })
                |> Maybe.withDefault bd



-- case bd.state of
--     Dragging _ _ ->
--         bd
--     _ ->
--         let
--             ctx =
--                 DragContext gd bd
--             vm =
--                 ViewModel.forBlock gd bd
--             component_ =
--                 case id.part of
--                     Component.Body ->
--                         vm |> Body.dragStart |> Maybe.map BodyDrag
--                     Component.Multiplicand ->
--                         vm |> MultiplicandControl.dragStart |> Maybe.map MultiplicandDrag
--                     Component.Multiplier ->
--                         vm |> MultiplierControl.dragStart |> Maybe.map MultiplierDrag
--                     Component.Offset ->
--                         vm |> OffsetControl.dragStart |> Maybe.map OffsetDrag
--                     Component.Quantity ->
--                         vm |> QuantityControl.dragStart |> Maybe.map QuantityDrag
--                     Component.Remainder ->
--                         vm |> Remainder.dragStart |> Maybe.map RemainderDrag
--                     Component.Width ->
--                         vm |> WidthControl.dragStart |> Maybe.map WidthDrag
--         in
--         component_
--             |> Maybe.map (\c -> { bd | state = Dragging ctx c })
--             |> Maybe.withDefault bd


dragMove : Delta -> Block -> Block
dragMove newDelta bd =
    case bd.drag of
        Just ( DragCtx ctx, drag ) ->
            let
                ( drag_, bd_ ) =
                    case drag of
                        BodyDrag state ->
                            state
                                |> Body.dragUpdate newDelta
                                |> Pair.fork BodyDrag (Body.dragMove ctx)

                        _ ->
                            ( drag, bd )
            in
            { bd_ | drag = Just ( DragCtx ctx, drag_ ) }

        Nothing ->
            bd



-- let
--     ( component_, bd_ ) =
--         case component of
--             BodyDrag state ->
--                 state
--                     |> Body.dragUpdate newDelta
--                     |> Pair.fork BodyDrag (Body.dragMove ctx)
--             MultiplicandDrag state ->
--                 state
--                     |> MultiplicandControl.dragUpdate newDelta
--                     |> Pair.fork
--                         MultiplicandDrag
--                         (MultiplicandControl.dragMove ctx)
--             MultiplierDrag state ->
--                 state
--                     |> MultiplierControl.dragUpdate newDelta
--                     |> Pair.fork
--                         MultiplierDrag
--                         (MultiplierControl.dragMove ctx)
--             OffsetDrag state ->
--                 state
--                     |> OffsetControl.dragUpdate newDelta
--                     |> Pair.fork OffsetDrag (OffsetControl.dragMove ctx)
--             QuantityDrag state ->
--                 state
--                     |> QuantityControl.dragUpdate newDelta
--                     |> Pair.fork QuantityDrag (QuantityControl.dragMove ctx)
--             RemainderDrag state ->
--                 state
--                     |> Remainder.dragUpdate newDelta
--                     |> Pair.fork RemainderDrag (Remainder.dragMove ctx)
--             WidthDrag state ->
--                 state
--                     |> WidthControl.dragUpdate newDelta
--                     |> Pair.fork WidthDrag (WidthControl.dragMove ctx)
-- in
-- { bd_ | state = Dragging ctx component_ }


endDrag : Block -> Maybe Block
endDrag bd =
    case bd.drag of
        Just ( DragCtx ctx, drag ) ->
            let
                bd_ =
                    case drag of
                        BodyDrag state ->
                            Body.dragEnd ctx state

                        _ ->
                            Just bd
            in
            bd_ |> Maybe.map (\b -> { b | drag = Nothing })

        Nothing ->
            Just bd



-- let
--     bd_ =
--         case component of
--             BodyDrag state ->
--                 Body.dragEnd ctx state
--             MultiplicandDrag state ->
--                 MultiplicandControl.dragEnd ctx state
--             MultiplierDrag state ->
--                 MultiplierControl.dragEnd ctx state
--             OffsetDrag state ->
--                 OffsetControl.dragEnd ctx state
--             QuantityDrag state ->
--                 QuantityControl.dragEnd ctx state
--             RemainderDrag state ->
--                 Remainder.dragEnd ctx state
--             WidthDrag state ->
--                 WidthControl.dragEnd ctx state
-- in
-- bd_ |> Maybe.map (\b -> { b | state = Selected })
