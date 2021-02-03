module Block exposing (init, initGroup, subscriptions, update, view)

import Block.Internal.Body as Body
import Block.Internal.Update exposing (..)
import Block.Internal.View as View
import Block.Internal.ViewModel as ViewModel
import Block.Internal.WidthControl as WidthControl
import Block.Model exposing (..)
import Cmd.Extra
import Draggable
import Draggable.Events
import Grid
import Html exposing (..)
import List
import ListEx
import MaybeEx
import Pair
import Svg
import Svg.Attributes as SvgAttrs
import Tuple



-- INIT


init :
    { key : String
    , xy : ( Int, Int )
    , quantity : Int
    , width : Int
    }
    -> Data
init input =
    { key = input.key
    , state = Idle
    , x = Tuple.first input.xy
    , y = Tuple.second input.xy
    , quantity = input.quantity
    , headerOffset = 0
    , width = input.width
    }


initGroup : (Msg -> msg) -> List Data -> Group msg
initGroup envelop blocks =
    { active = Nothing
    , context =
        { drag = Draggable.init
        , envelop = envelop
        }
    , rest = blocks
    }



-- SUBSCRIPTIONS


subscriptions : Context msg -> Sub msg
subscriptions context =
    Draggable.subscriptions (context.envelop << DragMsg) context.drag



-- UPDATE


update :
    Context msg
    -> Grid.Data
    -> Msg
    -> Maybe Data
    -> ( Maybe Data, Context msg, Cmd msg )
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
            ( model |> Maybe.map (startDrag id)
            , context
            , Cmd.none
            )

        DragMove delta ->
            ( model |> Maybe.map (dragMove delta gd)
            , context
            , Cmd.none
            )

        EndDrag ->
            ( model |> Maybe.map (endDrag gd)
            , context
            , Cmd.none
            )

        DragWidthControl delta ->
            ( model
            , context
            , Cmd.none
            )

        UpdateWidth ->
            ( model |> Maybe.map (endDrag gd)
            , context
            , Cmd.none
            )

        Select id ->
            ( model |> Maybe.map (\m -> { m | state = Selected })
            , context
            , Cmd.none
            )


handleDragMove : ( Int, Int ) -> Data -> Data
handleDragMove newDelta bd =
    let
        ( state_, cmd_ ) =
            case bd.state of
                Dragging Body oldDelta ->
                    ( Dragging Body (Body.updateDragMoveDelta oldDelta newDelta)
                    , Cmd.none
                    )

                Dragging WidthControl oldDelta ->
                    ( Dragging WidthControl (WidthControl.updateDragMoveDelta oldDelta newDelta)
                    , Cmd.none
                    )

                _ ->
                    ( bd.state, Cmd.none )
    in
    { bd | state = state_ }


dragConfig : (Msg -> msg) -> Draggable.Config Id msg
dragConfig envelop =
    Draggable.customConfig
        [ Draggable.Events.onDragStart <| envelop << StartDrag
        , Draggable.Events.onDragBy <| envelop << DragMove << Pair.map round
        , Draggable.Events.onDragEnd <| envelop EndDrag
        , Draggable.Events.onClick <| envelop << Select
        ]



-- VIEW


view : Context msg -> Grid.Data -> Data -> Svg.Svg msg
view context gd bd =
    let
        eventAttrsFn =
            eventAttrs context.envelop bd.key

        vm =
            ViewModel.forBlock gd bd

        elements =
            View.view eventAttrsFn vm

        -- controls =
        --     [ ListEx.last rectData
        --         |> MaybeEx.toMappedList
        --             (viewAddControl (eventsFn AddControl) (scaleFn AddControl))
        --     , List.head rectData
        --         |> MaybeEx.toMappedList
        --             (viewOffsetControl (eventsFn OffsetControl) (scaleFn OffsetControl))
        --     , List.head rectData
        --         |> MaybeEx.toMappedList
        --             (WidthControl.view (eventsFn WidthControl) (scaleFn WidthControl) bd)
        --     ]
        --         |> List.concat
    in
    Svg.g [ SvgAttrs.class "block" ] elements


eventAttrs : (Msg -> msg) -> String -> Part -> List (Svg.Attribute msg)
eventAttrs envelop key part =
    let
        id =
            Id key part
    in
    Draggable.mouseTrigger id (envelop << DragMsg)
        :: Draggable.touchTriggers id (envelop << DragMsg)


scaleForPart : Scale -> Data -> Part -> Scale
scaleForPart scale bd targetPart =
    case bd.state of
        Dragging part ( dx, dy ) ->
            if part == Body || part == targetPart then
                { scale | dx = scale.dx + dx, dy = scale.dy + dy }

            else
                scale

        _ ->
            scale
