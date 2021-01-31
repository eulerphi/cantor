module Main exposing (..)

import Block
import Block.Group
import Browser
import Browser.Dom
import Browser.Events
import Draggable
import Draggable.Events
import Grid
import Html exposing (div)
import Html.Attributes as HtmlAttrs
import Pair
import Svg
import Svg.Attributes as SvgAttrs
import Task
import Tuple



-- MODEL / INIT


type alias Model =
    { blocks : Block.Group
    , drag : Draggable.State String
    , grid : Grid.Data
    , margin : Int
    , size : ( Int, Int )
    }


type Msg
    = NoOp
    | Drag ( Int, Int )
    | DragMsg (Draggable.Msg String)
    | EndDragging
    | StartDragging String
    | SizeChanged ( Int, Int )
    | WindowResized


init : () -> ( Model, Cmd Msg )
init _ =
    let
        m =
            { blocks =
                { idle =
                    [ Block.data "1" 43 |> Block.withPos ( 5, 5 )
                    , Block.data "3" 36 |> Block.withPos ( 15, 15 )
                    ]
                , drag = Nothing
                }
            , drag = Draggable.init
            , grid = Grid.emptyParams
            , margin = 20
            , size = ( 0, 0 )
            }
    in
    ( m, changeSizeTask m )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> WindowResized)
        , Draggable.subscriptions DragMsg m.drag
        ]



-- VIEW


viewBlock : Grid.Data -> Block.Data -> Svg.Svg Msg
viewBlock gd bd =
    Block.view
        (Draggable.mouseTrigger bd.id DragMsg
            :: Draggable.touchTriggers bd.id DragMsg
        )
        gd
        bd


view : Model -> Html.Html Msg
view m =
    let
        idleBlocks =
            m.blocks.idle |> List.map (viewBlock m.grid)

        dragBlock =
            case m.blocks.drag of
                Nothing ->
                    []

                Just bd ->
                    [ viewBlock m.grid bd ]

        parts =
            [ Svg.g
                [ SvgAttrs.id "grid" ]
                (Grid.view m.grid)
            , Svg.g
                [ SvgAttrs.id "blocks" ]
                (idleBlocks ++ dragBlock)
            ]
    in
    div
        [ HtmlAttrs.id "root" ]
        [ Svg.svg
            [ SvgAttrs.width <| String.fromInt <| Tuple.first m.size
            , SvgAttrs.height <| String.fromInt <| Tuple.second m.size
            ]
            parts
        ]



--Update


changeSize : Model -> Browser.Dom.Element -> Msg
changeSize m e =
    let
        size =
            ( e.viewport.width, e.viewport.height )
                |> Pair.map (\x -> round x)
                |> Pair.map (\x -> x - (2 * m.margin))
    in
    SizeChanged size


changeSizeTask : Model -> Cmd Msg
changeSizeTask m =
    Task.attempt
        (\r ->
            case r of
                Ok e ->
                    changeSize m e

                Err _ ->
                    NoOp
        )
        (Browser.Dom.getElement "root")


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragBy (\delta -> Drag (Pair.map round delta))
        , Draggable.Events.onDragStart StartDragging
        , Draggable.Events.onDragEnd EndDragging
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        Drag delta ->
            ( { m | blocks = Block.Group.onDrag delta m.blocks }, Cmd.none )

        EndDragging ->
            ( { m | blocks = Block.Group.endDragging m.grid m.blocks }, Cmd.none )

        StartDragging id ->
            ( { m | blocks = Block.Group.startDragging id m.blocks }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg m

        SizeChanged wh ->
            let
                g =
                    Grid.centeredParams wh (Grid.calculateUnit wh 25)

                g_ =
                    { g | isAlternateLine = \idx -> idx == 2 || modBy 5 (idx - 2) == 0 }
            in
            ( { m | grid = g_, size = wh }, Cmd.none )

        WindowResized ->
            ( m, changeSizeTask m )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
