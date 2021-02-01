module Block.Group exposing (..)

import Block
import Block.Model exposing (..)
import Grid
import MaybeEx
import Svg


update : Grid.Data -> Msg -> Group msg -> ( Group msg, Cmd msg )
update gd msg model =
    case msg of
        StartDrag id ->
            let
                -- TODO : should validate that model.active is Nothing
                ( matches, rest_ ) =
                    List.partition (\bd -> bd.key == id.key) model.rest

                match =
                    List.head matches

                ( active_, context_, cmd_ ) =
                    Block.update model.context gd msg match
            in
            ( { model | active = active_, context = context_, rest = rest_ }, cmd_ )

        EndDrag ->
            let
                ( active_, context_, cmd_ ) =
                    Block.update model.context gd msg model.active

                rest_ =
                    active_
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                        |> (++) model.rest
            in
            ( { model | active = Nothing, context = context_, rest = rest_ }, cmd_ )

        _ ->
            let
                ( active_, context_, cmd_ ) =
                    Block.update model.context gd msg model.active
            in
            ( { model | active = active_, context = context_ }, cmd_ )


view : Scale -> Group msg -> List (Svg.Svg msg)
view scale group =
    let
        viewBlock =
            Block.view group.context scale

        idleBlocks =
            group.rest |> List.map viewBlock

        activeBlock =
            group.active |> Maybe.map viewBlock |> MaybeEx.toList
    in
    idleBlocks ++ activeBlock
