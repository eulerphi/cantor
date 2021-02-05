module Block.Internal.Update exposing (update)

import Block.Internal.Component as Component exposing (Component)
import Block.Internal.Components.Quantity as QuantityControl
import Block.Internal.Components.Width as WidthControl
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model as ViewModel exposing (ViewModel)
import Delta exposing (Delta)
import DragState exposing (DragState)
import Draggable
import Draggable.Events
import Grid
import MathEx
import Pair
import Pos exposing (Pos)


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
            ( model |> Maybe.map (startDrag id gd)
            , context
            , Cmd.none
            )

        DragMove delta ->
            ( model
            , context
            , Cmd.none
            )

        -- ( model |> Maybe.map (dragMove delta gd)
        -- , context
        -- , Cmd.none
        -- )
        EndDrag ->
            ( model |> Maybe.map (endDrag gd)
            , context
            , Cmd.none
            )

        Select id ->
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
    bd



-- let
--     vm =
--         ViewModel.forBlock gd bd
--     dragState =
--         case id.part of
--             Body ->
--                 initBodyDragState vm bd
--             _ ->
--                 initBodyDragState vm bd
-- in
-- { bd | state = Dragging id.part dragState }
-- initBodyDragState : ViewModel -> Data -> DragState
-- initBodyDragState vm bd =
--     { start = Pos.init ( 0, 0 )
--     , current = Delta.none
--     , total = Delta.none
--     }


endDrag : Grid.Data -> Block -> Block
endDrag gd bd =
    let
        dragDelta =
            case bd.state of
                Dragging Component.Body dragState ->
                    dragState.total

                _ ->
                    Delta.none

        ( dx, dy ) =
            dragDelta
                |> Delta.roundNear (toFloat gd.unit)
                |> Delta.map (\v -> round <| v / toFloat gd.unit)
    in
    { bd | x = bd.x + dx, y = bd.y + dy, state = Selected }


dragMove : ( Int, Int ) -> Grid.Data -> Block -> Block
dragMove newDelta gd bd =
    bd



-- let
--     block_ =
--         case bd.state of
--             Dragging QuantityControl dragState ->
--                 QuantityControl.dragMove
--                     (Pair.add oldDelta newDelta)
--                     gd
--                     bd
--             Dragging Body oldDelta ->
--                 { bd | state = Dragging Body (Pair.add oldDelta newDelta) }
--             Dragging WidthControl oldDelta ->
--                 WidthControl.dragMove
--                     (Pair.add oldDelta newDelta)
--                     gd
--                     bd
--             _ ->
--                 bd
-- in
-- block_
-- handleDragMove : ( Int, Int ) -> Data -> Data
-- handleDragMove newDelta bd =
--     let
--         ( state_, cmd_ ) =
--             case bd.state of
--                 Dragging Body oldDelta ->
--                     ( Dragging Body (Body.updateDragMoveDelta oldDelta newDelta)
--                     , Cmd.none
--                     )
--                 Dragging WidthControl oldDelta ->
--                     ( Dragging WidthControl (WidthControl.updateDragMoveDelta oldDelta newDelta)
--                     , Cmd.none
--                     )
--                 _ ->
--                     ( bd.state, Cmd.none )
--     in
--     { bd | state = state_ }
