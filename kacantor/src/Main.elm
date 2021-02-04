module Main exposing (..)

import Block
import Block.Group
import Browser
import Browser.Dom
import Browser.Events
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
    { blocks : Block.Group Msg
    , grid : Grid.Data
    , margin : Int
    , size : ( Int, Int )
    }


type Msg
    = NoOp
    | SizeChanged ( Int, Int )
    | WindowResized
    | BlockMsg Block.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    let
        m =
            { blocks =
                Block.initGroup
                    BlockMsg
                    [ Block.initBlock { key = "1", xy = ( 5, 5 ), quantity = 43, width = 10 }
                    , Block.initBlock { key = "3", xy = ( 15, 15 ), quantity = 36, width = 10 }
                    ]
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
        , Block.subscriptions m.blocks.context
        ]



-- VIEW


view : Model -> Html.Html Msg
view m =
    let
        parts =
            [ Svg.g
                [ SvgAttrs.id "grid" ]
                (Grid.view m.grid)
            , Svg.g
                [ SvgAttrs.id "blocks" ]
                (Block.Group.view m.grid m.blocks)
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
                    Block.Group.update m.grid subMsg m.blocks
            in
            ( { m | blocks = blocks_ }, cmd_ )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
