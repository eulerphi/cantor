module Block exposing (Config, Data, DragState, Group, Msg(..), data, emptyData, endDrag, initDragState, onDrag, startDrag, subscriptions, update, view, view2, viewAll, withPos)

import Draggable
import Draggable.Events exposing (onDragEnd)
import Extra
import Grid
import Html exposing (..)
import List
import Pair
import Svg
import Svg.Attributes as SvgAttrs
import Tuple exposing (..)


type alias Data =
    { id : String
    , drag : Maybe ( Int, Int )
    , x : Int
    , y : Int
    , quantity : Int
    , headerOffset : Int
    , width : Int
    }


emptyData : Data
emptyData =
    { id = ""
    , drag = Nothing
    , x = 0
    , y = 0
    , quantity = 0
    , headerOffset = 0
    , width = 0
    }


type alias Group =
    { idle : List Data
    , drag : Maybe Data
    , dragState : DragState
    }


type State
    = Idle
    | Selected
    | Dragging


type Msg
    = DragMsg (Draggable.Msg Id)
    | StartDrag Id
    | DragMove ( Int, Int )
    | EndDrag


type alias Config msg =
    { grid : Grid.Data
    , envelopFn : Msg -> msg
    }


subscriptions : (Msg -> msg) -> DragState -> Sub msg
subscriptions envelopFn state =
    Draggable.subscriptions (\subMsg -> envelopFn (DragMsg subMsg)) state.drag


type Type
    = Block


type alias Id =
    ( String, Type )



-- | WidthControl String
-- | HeaderOffsetControl String
-- | AddOrSubtractControl String


type alias DragState =
    { drag : Draggable.State Id
    , dragId : Maybe Id
    , delta : ( Int, Int )
    }


initDragState : DragState
initDragState =
    { drag = Draggable.init
    , dragId = Nothing
    , delta = ( 0, 0 )
    }


draggableConfig : (Msg -> msg) -> Draggable.Config Id msg
draggableConfig envelop =
    Draggable.customConfig
        [ Draggable.Events.onDragBy <| envelop << DragMove << Pair.map round
        , Draggable.Events.onDragStart <| envelop << StartDrag
        , Draggable.Events.onDragEnd <| envelop EndDrag
        ]


update :
    Config msg
    -> Msg
    -> DragState
    -> Maybe Data
    -> ( Maybe Data, DragState, Cmd msg )
update config msg dragState model =
    case msg of
        DragMsg subMsg ->
            let
                ( dragState_, cmd_ ) =
                    Draggable.update (draggableConfig config.envelopFn) subMsg dragState
            in
            ( model, dragState_, cmd_ )

        StartDrag id ->
            ( model |> Maybe.map startDrag, dragState, Cmd.none )

        DragMove delta ->
            ( model |> Maybe.map (onDrag delta)
            , { dragState | delta = Pair.add dragState.delta delta }
            , Cmd.none
            )

        EndDrag ->
            ( model |> Maybe.map (endDrag config.grid)
            , { dragState | delta = ( 0, 0 ), dragId = Nothing }
            , Cmd.none
            )


data : String -> Int -> Data
data id quantity =
    { id = id
    , drag = Nothing
    , x = 0
    , y = 0
    , quantity = quantity
    , headerOffset = 0
    , width = min quantity 10
    }


endDrag : Grid.Data -> Data -> Data
endDrag gd bd =
    let
        ( dx, dy ) =
            bd.drag
                |> Maybe.withDefault ( 0, 0 )
                |> Pair.map (Extra.roundNear gd.unit)
                |> Pair.map (\v -> v // gd.unit)
    in
    { bd | x = bd.x + dx, y = bd.y + dy, drag = Nothing }


startDrag : Data -> Data
startDrag bd =
    { bd | drag = Just ( 0, 0 ) }


onDrag : ( Int, Int ) -> Data -> Data
onDrag delta bd =
    let
        drag =
            bd.drag |> Maybe.map (Pair.add delta)
    in
    { bd | drag = drag }


withPos : ( Int, Int ) -> Data -> Data
withPos pos bd =
    { bd | x = Tuple.first pos, y = Tuple.second pos }


eventAttrs : (Msg -> msg) -> Id -> List (Svg.Attribute msg)
eventAttrs envelop id =
    Draggable.mouseTrigger id (envelop << DragMsg)
        :: Draggable.touchTriggers id (envelop << DragMsg)


view : List (Svg.Attribute msg) -> Grid.Data -> Data -> Svg.Svg msg
view attrs gd bd =
    let
        rectData =
            toLogicalViewData bd |> List.map (toPhysicalViewData gd)

        rects =
            rectData |> List.map (rect gd bd)

        controls =
            [ viewWidthControl gd bd ]
    in
    Svg.g
        [ SvgAttrs.class "block" ]
        (rects ++ controls)


view2 : (Msg -> msg) -> Grid.Data -> Data -> Svg.Svg msg
view2 enevelop gd bd =
    let
        rectData =
            toLogicalViewData bd |> List.map (toPhysicalViewData gd)

        rects =
            rectData |> List.map (rect gd bd)

        controls =
            [ viewWidthControl gd bd ]

        blockEventAttrs_ =
            eventAttrs enevelop ( bd.id, Block )
    in
    Svg.g
        (SvgAttrs.class "block" :: blockEventAttrs_)
        (rects ++ controls)


viewAll : List (Svg.Attribute msg) -> Grid.Data -> List Data -> List (Svg.Svg msg)
viewAll attrs gd bds =
    bds |> List.map (view attrs gd)


viewWidthControl : Grid.Data -> Data -> Svg.Svg msg
viewWidthControl gd bd =
    let
        ( x, y ) =
            ( bd.x, bd.y )
                |> Pair.map ((*) gd.unit)
                |> Tuple.mapBoth ((+) gd.x) ((+) gd.y)
                |> Tuple.mapBoth ((+) (gd.unit * bd.width)) ((+) (gd.unit // -2))

        ( dx, dy ) =
            dragDelta bd
    in
    Svg.g
        []
        [ Svg.circle
            [ SvgAttrs.cx <| String.fromInt <| x + dx
            , SvgAttrs.cy <| String.fromInt <| y + dy
            , SvgAttrs.r <| String.fromInt <| gd.unit // 3
            , SvgAttrs.fill "rgb(85,209,229)"
            , SvgAttrs.stroke "rgb(85,209,229)"
            , SvgAttrs.cursor "w-resize"
            ]
            []
        ]


dragDelta : Data -> ( Int, Int )
dragDelta bd =
    Maybe.withDefault ( 0, 0 ) bd.drag



-- VIEW DATA


type alias ViewData =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , class : String
    }


toLogicalViewData : Data -> List ViewData
toLogicalViewData bd =
    let
        ( headerWidth, headerHeight ) =
            if bd.headerOffset > 0 && bd.width > bd.headerOffset then
                ( bd.width - bd.headerOffset, 1 )

            else
                ( 0, 0 )

        header =
            { x = bd.x + bd.headerOffset
            , y = bd.y
            , width = headerWidth
            , height = headerHeight
            , class = "block-header"
            }

        body =
            { x = bd.x
            , y = bd.y + header.height
            , width = bd.width
            , height = (bd.quantity - header.width) // bd.width
            , class = "block-body"
            }

        remainder =
            bd.quantity - header.width - (body.width * body.height)

        footer =
            { x = bd.x
            , y = bd.y + header.height + body.height
            , width = remainder
            , height = 1
            , class = "block-footer"
            }
    in
    [ header, body, footer ]
        |> List.filter (\vd -> vd.width > 0)


toPhysicalViewData : Grid.Data -> ViewData -> ViewData
toPhysicalViewData gd vd =
    { x = gd.unit * vd.x + gd.x
    , y = gd.unit * vd.y + gd.y
    , width = gd.unit * vd.width
    , height = gd.unit * vd.height
    , class = vd.class
    }



-- RECT


rect : Grid.Data -> Data -> ViewData -> Svg.Svg msg
rect gd bd vd =
    let
        ( dx, dy ) =
            Maybe.withDefault ( 0, 0 ) bd.drag
    in
    Svg.g
        [ SvgAttrs.class vd.class ]
        (Grid.view
            { x = vd.x + dx
            , y = vd.y + dy
            , width = vd.width
            , height = vd.height
            , unit = gd.unit
            , isAlternateLine = \_ -> False
            }
        )
