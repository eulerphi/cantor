module Main exposing (..)

import Block
import Block.Group as Group exposing (Group)
import Browser
import CircleButton
import Grid exposing (Grid)
import Html exposing (Html)
import Pair
import Pos exposing (Pos)
import Size
import Svg
import Svg.Attributes as SvgAttrs
import Svg.Events as SvgEvts
import ViewContext exposing (ViewContext)



-- MODEL / INIT


type alias Model =
    { blocks : Group Msg
    , grid : Grid
    , gridOpts : Grid.Options
    , key : Int
    , viewCtx : ViewContext Msg
    }


type Msg
    = NoOp
    | AddBlock
    | ClearSelection
    | BlockMsg Block.Msg
    | ViewCtxMsg ViewContext.Msg


init : Float -> ( Model, Cmd Msg )
init devicePixelRatio =
    let
        m =
            { blocks = Group.init BlockMsg []
            , grid = Grid.emptyGrid
            , gridOpts = Grid.Options isAlternateLine isAlternateLine
            , key = 0
            , viewCtx =
                ViewContext.init
                    { devicePixelRatio = devicePixelRatio
                    , elementId = "root"
                    , envelope = ViewCtxMsg
                    , padding = 10
                    }
            }
    in
    ( m, ViewContext.initCmd m.viewCtx )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ Block.subscriptions m.blocks.context
        , ViewContext.subscriptions m.viewCtx
        ]



-- VIEW


view : Model -> Html Msg
view m =
    Svg.svg
        [ SvgAttrs.width <| Size.toWidthString m.viewCtx.size
        , SvgAttrs.height <| Size.toHeightString m.viewCtx.size
        ]
        [ Grid.viewWithOptions
            [ SvgAttrs.id "grid"
            , SvgEvts.onClick ClearSelection
            ]
            m.gridOpts
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



-- UPDATE


isAlternateLine : Int -> Bool
isAlternateLine idx =
    modBy 2 idx == 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        AddBlock ->
            let
                key_ =
                    m.key + 1

                pos =
                    m.grid.size
                        |> Size.map (\v -> v / 2)
                        |> Size.toPair
                        |> Pair.uncurry Pos
                        |> Pos.roundNear m.grid
                        |> Pos.addX -(3 * m.grid.unit)

                newBlock =
                    Block.init
                        { key = String.fromInt key_
                        , pos = pos
                        , quantity = 5
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

        BlockMsg subMsg ->
            let
                ( blocks_, cmd_ ) =
                    Group.update m.grid subMsg m.blocks
            in
            ( { m | blocks = blocks_ }, cmd_ )

        ViewCtxMsg subMsg ->
            let
                ( vc_, cmd_ ) =
                    ViewContext.update subMsg m.viewCtx
            in
            ( { m | grid = Grid.forViewContext 25 vc_, viewCtx = vc_ }, cmd_ )



-- MAIN


main : Program Float Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
