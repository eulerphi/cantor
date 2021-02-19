module ViewContext exposing
    ( Msg
    , ViewContext
    , ViewContextLike
    , init
    , initCmd
    , subscriptions
    , update
    )

import Browser.Dom
import Browser.Events
import Size exposing (Size)
import Task


type alias ViewContext msg =
    { devicePixelRatio : Float
    , elementId : String
    , envelope : Msg -> msg
    , padding : Float
    , size : Size
    }


type alias ViewContextLike r =
    { r
        | devicePixelRatio : Float
        , padding : Float
        , size : Size
    }


type Msg
    = NoOp
    | ViewportChanged Browser.Dom.Viewport
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

        ViewportChanged e ->
            ( vc |> updateOnSizeChanged e, Cmd.none )

        WindowResized ->
            ( vc, getElementCmd vc )



-- PRIVATE


getElementCmd : ViewContext msg -> Cmd msg
getElementCmd vc =
    Task.perform (vc.envelope << ViewportChanged) Browser.Dom.getViewport


updateOnSizeChanged : Browser.Dom.Viewport -> ViewContext msg -> ViewContext msg
updateOnSizeChanged { viewport } vc =
    { vc
        | size =
            Size viewport.width viewport.height
                |> Size.addWidth -(2 * vc.padding)
                |> Size.addHeight -(2 * vc.padding)
    }
