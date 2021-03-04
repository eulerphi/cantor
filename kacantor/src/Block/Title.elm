module Block.Title exposing (view)

import Block exposing (Block(..), Msg(..))
import Block.Internal.Section as Section
import Block.Internal.Types exposing (DragComponent(..), State(..))
import Block.Internal.ViewModel as ViewModel exposing (ViewModel)
import Box exposing (Box)
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes as HtmlAttrs
import List
import Pair
import Pos
import Size exposing (Size)
import String
import StringEx
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs
import SvgEx


view : Grid -> Maybe Block -> Maybe (Svg msg)
view gd block =
    block
        |> Maybe.map (\(Block bd) -> ViewModel.forBlock gd bd)
        |> Maybe.map viewTitle


viewTitle : ViewModel -> Svg msg
viewTitle vm =
    let
        box =
            titleBox vm.grid
    in
    Svg.g
        [ SvgAttrs.class "block-title" ]
        [ SvgEx.foreignObject
            []
            box
            [ Html.div
                [ HtmlAttrs.class "container"
                , HtmlAttrs.style "font-size" <| fontSize box
                , HtmlAttrs.style "height" <| Size.toPixelHeightString box.size
                ]
                (getTitleHtml vm)
            ]
        ]


getTitleHtml : ViewModel -> List (Html msg)
getTitleHtml vm =
    case vm.block.state of
        Dragging { bd } (DragWidth _) ->
            let
                ( newQuantity, oldQuantity ) =
                    ( vm.block.quantity, bd.quantity )

                delta =
                    newQuantity - oldQuantity
            in
            [ Html.span
                [ HtmlAttrs.class "sum" ]
                [ Html.text <| String.fromInt newQuantity ]
            , Html.span
                [ HtmlAttrs.class "operator" ]
                [ Html.text " = " ]
            , Html.span
                [ HtmlAttrs.class "sum" ]
                [ Html.text <| String.fromInt oldQuantity ]
            , Html.span
                [ HtmlAttrs.class "operator" ]
                [ Html.text " + " ]
            , Html.span
                [ HtmlAttrs.class "active" ]
                [ Html.text <| String.fromInt delta ]
            ]

        _ ->
            let
                lhs =
                    [ Html.span
                        [ HtmlAttrs.class "sum" ]
                        [ Html.text <| String.fromInt <| vm.block.quantity ]
                    , Html.span
                        [ HtmlAttrs.class "operator" ]
                        [ Html.text " = " ]
                    ]

                rhs =
                    vm.sections
                        |> List.map
                            (\s ->
                                Html.span
                                    [ HtmlAttrs.class s.class ]
                                    [ Html.text <| Section.titleText s ]
                            )
                        |> List.intersperse
                            (Html.span [ HtmlAttrs.class "operator" ] [ Html.text " + " ])
            in
            lhs ++ rhs


fontSize : Box -> String
fontSize box =
    let
        padding =
            8

        factor =
            3 / 4
    in
    (box.size.height - padding)
        |> (*) factor
        |> round
        |> String.fromInt
        |> StringEx.prepend "px"


titleBox : Grid -> Box
titleBox gd =
    Size gd.size.width (2 * gd.unit)
        |> Pair.fork
            (\s ->
                gd.pos
                    |> Pos.addX (gd.size.width / 2)
                    |> Pos.addX -(s.width / 2)
            )
            identity
        |> Pair.uncurry Box
        |> Box.pad (gd.unit / 4)
