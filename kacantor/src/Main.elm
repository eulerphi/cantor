module Main exposing (..)

import Block
import Block.Group as Group exposing (Group)
import Browser
import Browser.Dom
import Browser.Events
import CircleButton
import Grid
import Html exposing (div)
import Html.Attributes as HtmlAttrs
import Pair
import Pos
import Size
import Svg
import Svg.Attributes as SvgAttrs
import Svg.Events as SvgEvts
import Task



-- MODEL / INIT


type alias Model =
    { blocks : Group Msg
    , devicePixelRatio : Float
    , grid : Grid.Data
    , key : Int
    , margin : Int
    , size : ( Int, Int )
    }


type Msg
    = NoOp
    | AddBlock
    | ClearSelection
    | SizeChanged ( Int, Int )
    | WindowResized
    | BlockMsg Block.Msg


init : Float -> ( Model, Cmd Msg )
init devicePixelRatio =
    let
        m =
            { blocks = Group.init BlockMsg []
            , devicePixelRatio = devicePixelRatio
            , grid = Grid.emptyParams
            , key = 0
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
        , Block.subscriptions m.blocks.context
        ]



-- VIEW


view : Model -> Html.Html Msg
view m =
    let
        windowSize =
            Size.fromInt m.size
    in
    div
        [ HtmlAttrs.id "root" ]
        [ Svg.svg
            [ SvgAttrs.width <| Size.toWidthString windowSize
            , SvgAttrs.height <| Size.toHeightString windowSize
            ]
            [ Grid.view
                [ SvgAttrs.id "grid"
                , SvgEvts.onClick ClearSelection
                ]
                m.grid
            , Svg.g
                [ SvgAttrs.id "blocks" ]
                (Group.view m.grid m.blocks)
            , CircleButton.view
                [ SvgAttrs.id "add-block-btn"
                , SvgEvts.onClick AddBlock
                ]
                m.grid
                "+"
            ]
        ]



-- UPDATE


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


isAlternateLine : Int -> Bool
isAlternateLine idx =
    idx == 2 || modBy 5 (idx - 2) == 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        AddBlock ->
            let
                key_ =
                    m.key + 1

                ( gridPos, gridUnit ) =
                    ( Pos.fromInt ( m.grid.x, m.grid.y )
                    , toFloat m.grid.unit
                    )

                { x, y } =
                    m.size
                        |> Pair.map (\v -> v // 2)
                        |> Pos.fromInt
                        |> Pos.roundNear { pos = gridPos, unit = gridUnit }

                newBlock =
                    Block.init
                        { key = String.fromInt key_
                        , pos = ( round x, round y )
                        , quantity = 10
                        , width = 10
                        }

                blocks_ =
                    Group.addBlock newBlock m.blocks
            in
            ( { m | blocks = blocks_, key = key_ }, Cmd.none )

        ClearSelection ->
            let
                blocks_ =
                    Group.clearSelection m.blocks
            in
            ( { m | blocks = blocks_ }, Cmd.none )

        SizeChanged wh ->
            let
                g =
                    Grid.centeredParams wh (Grid.calculateUnit wh 25)
            in
            ( { m | grid = { g | isAlternateLine = isAlternateLine }, size = wh }
            , Cmd.none
            )

        WindowResized ->
            ( m, changeSizeTask m )

        BlockMsg subMsg ->
            let
                ( blocks_, cmd_ ) =
                    Group.update m.grid subMsg m.blocks
            in
            ( { m | blocks = blocks_ }, cmd_ )



-- MAIN


main : Program Float Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
