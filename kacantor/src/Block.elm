module Block exposing
    ( Block(..)
    , Context(..)
    , Msg(..)
    , clearSelection
    , context
    , init
    , subscriptions
    , update
    , view
    )

import Block.Internal.Types as Types
import Block.Internal.Update as Update
import Block.Internal.View as View
import Draggable
import Grid exposing (Grid)
import Html exposing (..)
import Pos exposing (Pos)
import Svg



-- TYPES


type Block
    = Block Types.Block


type Context msg
    = Context (Types.Context msg)


type Msg
    = Msg Types.Msg



-- INIT


init :
    { key : String
    , pos : Pos
    , quantity : Int
    , width : Int
    }
    -> Block
init input =
    let
        height =
            input.quantity // input.width

        remainder =
            input.quantity - (height * input.width)
    in
    Block
        { key = input.key
        , state = Types.Idle
        , pos = input.pos
        , quantity = input.quantity
        , headerOffset = 0
        , width = input.width
        , height = height
        , remainder = remainder
        }


context : (Msg -> msg) -> Context msg
context envelope =
    Context
        { drag = Draggable.init
        , envelop = \submsg -> envelope (Msg submsg)
        }



-- SUBSCRIPTIONS


subscriptions : Context msg -> Sub msg
subscriptions (Context ctx) =
    Draggable.subscriptions (ctx.envelop << Types.DragMsg) ctx.drag



-- UPDATE


update :
    Context msg
    -> Grid
    -> Msg
    -> Maybe Block
    -> ( Maybe Block, Context msg, Cmd msg )
update (Context ctx) gd (Msg msg) block =
    let
        bd =
            block |> Maybe.map (\(Block b) -> b)

        ( bd_, ctx_, cmd_ ) =
            Update.update ctx gd msg bd

        block_ =
            bd_ |> Maybe.map (\b -> Block b)
    in
    ( block_, Context ctx_, cmd_ )


clearSelection : Block -> Block
clearSelection (Block bd) =
    Block (bd |> Update.clearSelection)



-- VIEW


view :
    Context msg
    -> Grid
    -> Block
    -> Svg.Svg msg
view (Context ctx) gd (Block bd) =
    View.view ctx gd bd
