module Block exposing (init, initGroup, subscriptions, update, view)

import Block.Internal.Update exposing (..)
import Block.Internal.View exposing (..)
import Block.Model exposing (..)
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
            ( model |> Maybe.map (onDrag delta)
            , context
            , Cmd.none
            )

        EndDrag ->
            ( model |> Maybe.map (endDrag gd)
            , context
            , Cmd.none
            )


dragConfig : (Msg -> msg) -> Draggable.Config Id msg
dragConfig envelop =
    Draggable.customConfig
        [ Draggable.Events.onDragStart <| envelop << StartDrag
        , Draggable.Events.onDragBy <| envelop << DragMove << Pair.map round
        , Draggable.Events.onDragEnd <| envelop EndDrag
        ]



-- VIEW


view : Context msg -> Scale -> Data -> Svg.Svg msg
view context scale bd =
    let
        scaleFn =
            scaleForPart scale bd

        eventsFn =
            eventAttrs context.envelop bd.key

        rectData =
            toRectData bd

        rects =
            rectData |> List.map (viewRect <| scaleFn Body)

        controls =
            [ ListEx.last rectData
                |> MaybeEx.toMappedList
                    (viewAddControl (eventsFn AddControl) (scaleFn AddControl))
            , List.head rectData
                |> MaybeEx.toMappedList
                    (viewOffsetControl (eventsFn OffsetControl) (scaleFn OffsetControl))
            , List.head rectData
                |> MaybeEx.toMappedList
                    (viewWidthControl (eventsFn WidthControl) (scaleFn WidthControl))
            ]
                |> List.concat
    in
    Svg.g
        [ SvgAttrs.class "block" ]
        [ Svg.g (eventsFn Body) rects
        , Svg.g [] controls
        ]


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
