module Block.Group exposing (..)

import Block
import Block.Model exposing (..)
import Draggable
import Grid
import MaybeEx
import Svg


partition :
    Id
    -> ( Maybe Data, List Data )
    -> ( Maybe Data, List Data )
partition id ( active, rest ) =
    case active of
        Just data ->
            if data.key == id.key then
                ( active, rest )

            else
                partition id ( Nothing, rest ++ [ { data | state = Idle } ] )

        Nothing ->
            let
                ( matches, rest_ ) =
                    List.partition (\bd -> bd.key == id.key) rest

                match =
                    List.head matches
            in
            ( match, rest_ )


subscriptions : Block.Group msg -> Sub msg
subscriptions group =
    Block.subscriptions group.context


update : Grid.Data -> Block.Msg -> Block.Group msg -> ( Block.Group msg, Cmd msg )
update gd msg model =
    ( model, Cmd.none )



-- case msg of
--     StartDrag id ->
--         let
--             ( active_, rest_ ) =
--                 partition id ( model.active, model.rest )
--             ( active__, context_, cmd_ ) =
--                 Block.update model.context gd msg active_
--         in
--         ( { model | active = active__, context = context_, rest = rest_ }, cmd_ )
--     Select id ->
--         let
--             ( active_, rest_ ) =
--                 partition id ( model.active, model.rest )
--             ( active__, context_, cmd_ ) =
--                 Block.update model.context gd msg active_
--         in
--         ( { model | active = active__, context = context_, rest = rest_ }, cmd_ )
--     _ ->
--         let
--             ( active_, context_, cmd_ ) =
--                 Block.update model.context gd msg model.active
--         in
--         ( { model | active = active_, context = context_ }, cmd_ )


view : Grid.Data -> Block.Group msg -> List (Svg.Svg msg)
view gd group =
    []



-- let
--     viewBlock =
--         Block.view group.context gd
--     idleBlocks =
--         group.rest |> List.map viewBlock
--     activeBlock =
--         group.active |> Maybe.map viewBlock |> MaybeEx.toList
-- in
-- idleBlocks ++ activeBlock
