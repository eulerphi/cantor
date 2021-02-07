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
import Grid
import Html exposing (..)
import Pos
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
    , pos : ( Int, Int )
    , quantity : Int
    , width : Int
    }
    -> Block
init input =
    Block
        { key = input.key
        , state = Types.Idle
        , pos = Pos.fromInt input.pos
        , quantity = input.quantity
        , headerOffset = 0
        , width = input.width
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
    -> Grid.Data
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
    Block { bd | state = Types.Idle }



-- VIEW


view :
    Context msg
    -> Grid.Data
    -> Block
    -> Svg.Svg msg
view (Context ctx) gd (Block bd) =
    View.view ctx gd bd
