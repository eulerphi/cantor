module Block.Internal.Components.Body exposing (..)

import Block.Internal.Component as Component exposing (Component)
import Block.Internal.Types exposing (..)
import Block.Internal.View.Model exposing (ViewModel)
import Grid
import List
import Maybe.Extra
import Svg exposing (Attribute, Svg)
import Svg.Attributes as SvgAttrs
import ViewData exposing (ViewData)


dragMove : Grid.Data -> Block -> Block
dragMove gd bd =
    case bd.state of
        Dragging Component.Body dragState ->
            bd

        _ ->
            bd


view : List (Attribute msg) -> ViewModel -> Svg msg
view eventAttrs vm =
    let
        viewFn =
            viewRect vm

        optional =
            [ vm.body.top, vm.body.bot ]
                |> List.map (Maybe.map viewFn)

        elements =
            (Just (viewFn vm.body.mid) :: optional)
                |> Maybe.Extra.values
    in
    Svg.g
        (SvgAttrs.class "block-body" :: eventAttrs)
        elements


viewRect : ViewModel -> ViewData -> Svg.Svg msg
viewRect vm vd =
    let
        vd_ =
            vd |> ViewData.addPos vm.block
    in
    Svg.g
        [ SvgAttrs.class vd.class ]
        (Grid.view (Grid.forViewData vm.grid.unit vd_))
