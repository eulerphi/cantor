module Block exposing (Data, Group, Msg, data, endDrag, onDrag, startDrag, view, viewAll, withPos)

import Draggable
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


type alias Group =
    { idle : List Data
    , drag : Maybe Data
    }


type State
    = Idle
    | Selected
    | Dragging


type Msg
    = Drag ( Int, Int )
    | DragMsg (Draggable.Msg String)
    | EndDragging
    | StartDragging String


type alias Config msg =
    { envelopFn : Msg -> msg
    }


update :
    Config msg
    -> Msg
    -> Data
    -> ( Data, Cmd msg )
update envelopFn msg model =
    ( model, Cmd.none )


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
        (SvgAttrs.class "block" :: attrs)
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
