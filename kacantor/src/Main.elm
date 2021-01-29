module Main exposing (..)

import Block
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
    { blocks : List Block.Data
    , grid : Grid.Data
    , margin : Int
    , size : ( Int, Int )
    }


type Msg
    = NoOp
    | SizeChanged ( Int, Int )
    | WindowResized


init : () -> ( Model, Cmd Msg )
init _ =
    let
        m =
            { blocks =
                [ Block.data "1" 36 |> Block.withPos ( 5, 5 )
                , Block.data "2" 54 |> Block.withPos ( 20, 5 )
                ]
            , grid = Grid.emptyParams
            , margin = 20
            , size = ( 0, 0 )
            }
    in
    ( m, changeSizeTask m )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\_ _ -> WindowResized)



-- VIEW


view : Model -> Html.Html Msg
view m =
    let
        parts =
            [ Grid.view m.grid
            , Block.viewAll m.grid m.blocks
            ]
    in
    div
        [ HtmlAttrs.id "root" ]
        [ Svg.svg
            [ SvgAttrs.width <| String.fromInt <| Tuple.first m.size
            , SvgAttrs.height <| String.fromInt <| Tuple.second m.size
            ]
            (List.concat parts)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        SizeChanged wh ->
            let
                g =
                    Grid.centeredParams wh (Grid.getOffset wh 15)

                g_ =
                    { g | alternateCount = 5 }
            in
            ( { m | grid = g_, size = wh }, Cmd.none )

        WindowResized ->
            ( m, changeSizeTask m )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
