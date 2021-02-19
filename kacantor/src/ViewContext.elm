module ViewContext exposing
    ( Msg
    , ViewContext
    , init
    , initCmd
    , subscriptions
    , update
    )

import Browser.Dom
import Browser.Events
import Pos exposing (Pos)
import Size exposing (Size)
import Task


type alias ViewContext msg =
    { devicePixelRatio : Float
    , elementId : String
    , envelope : Msg -> msg
    , padding : Float
    , size : Size
    }


type Msg
    = NoOp
    | ElementChanged Browser.Dom.Element
    | WindowResized



-- INIT


init :
    { devicePixelRatio : Float
    , elementId : String
    , envelope : Msg -> msg
    , padding : Float
    }
    -> ViewContext msg
init input =
    ViewContext
        input.devicePixelRatio
        input.elementId
        input.envelope
        input.padding
        Size.none


initCmd : ViewContext msg -> Cmd msg
initCmd vc =
    getElementCmd vc



-- SUBSCRIPTIONS


subscriptions : ViewContext msg -> Sub msg
subscriptions vc =
    Browser.Events.onResize (\_ _ -> WindowResized |> vc.envelope)


update :
    Msg
    -> ViewContext msg
    -> ( ViewContext msg, Cmd msg )
update msg vc =
    case msg of
        NoOp ->
            ( vc, Cmd.none )

        ElementChanged e ->
            ( vc |> updateOnSizeChanged e, Cmd.none )

        WindowResized ->
            ( vc, getElementCmd vc )



-- PRIVATE


getElementCmd : ViewContext msg -> Cmd msg
getElementCmd vc =
    Task.attempt
        (\r ->
            case r of
                Ok e ->
                    e |> ElementChanged |> vc.envelope

                Err _ ->
                    NoOp |> vc.envelope
        )
        (Browser.Dom.getElement vc.elementId)


updateOnSizeChanged : Browser.Dom.Element -> ViewContext msg -> ViewContext msg
updateOnSizeChanged e vc =
    { vc
        | size =
            Size e.viewport.width e.viewport.height
                |> Size.addWidth -(2 * vc.padding)
                |> Size.addHeight -(2 * vc.padding)
    }
