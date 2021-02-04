module Block exposing (Block(..), Context(..), Group, Msg(..), initBlock, initContext, initGroup, subscriptions, update, view)

import Block.Internal.Update as Update
import Block.Internal.View as View
import Block.Model as Model
import Draggable
import Grid
import Html exposing (..)
import Svg
import Tuple


type Block
    = Block Model.Data


type Context msg
    = Context (Model.Context msg)


type alias Group msg =
    { active : Maybe Block
    , context : Context msg
    , rest : List Block
    }


type Msg
    = Msg Model.Msg



-- INIT


initBlock :
    { key : String
    , xy : ( Int, Int )
    , quantity : Int
    , width : Int
    }
    -> Block
initBlock input =
    Block
        { key = input.key
        , state = Model.Idle
        , x = Tuple.first input.xy
        , y = Tuple.second input.xy
        , quantity = input.quantity
        , headerOffset = 0
        , width = input.width
        }


initContext : (Msg -> msg) -> Context msg
initContext envelope =
    Context
        { drag = Draggable.init
        , envelop = \submsg -> envelope (Msg submsg)
        }


initGroup : (Msg -> msg) -> List Block -> Group msg
initGroup envelope blocks =
    { active = Nothing
    , context = initContext envelope
    , rest = blocks
    }



-- SUBSCRIPTIONS


subscriptions : Context msg -> Sub msg
subscriptions (Context ctx) =
    Draggable.subscriptions (ctx.envelop << Model.DragMsg) ctx.drag



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
    ( block_, Context ctx, cmd_ )



-- VIEW


view :
    Context msg
    -> Grid.Data
    -> Block
    -> Svg.Svg msg
view (Context ctx) gd (Block bd) =
    View.view ctx gd bd
