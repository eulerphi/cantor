module Block.Group exposing
    ( Group
    , addBlock
    , clearSelection
    , init
    , subscriptions
    , update
    , view
    )

import Block exposing (Block(..), Context, Msg(..))
import Block.Internal.Types as Types
import Grid exposing (Grid)
import Maybe.Extra as MaybeEx
import Svg


type alias Group msg =
    { active : Maybe Block
    , context : Context msg
    , rest : List Block
    }


init : (Msg -> msg) -> List Block -> Group msg
init envelope blocks =
    { active = Nothing
    , context = Block.context envelope
    , rest = blocks
    }


subscriptions : Group msg -> Sub msg
subscriptions group =
    Block.subscriptions group.context


update : Grid -> Msg -> Group msg -> ( Group msg, Cmd msg )
update gd msg model =
    case msg of
        Msg (Types.StartDrag id) ->
            let
                ( active_, rest_ ) =
                    partition id ( model.active, model.rest )

                ( active__, context_, cmd_ ) =
                    Block.update model.context gd msg active_
            in
            ( { model | active = active__, context = context_, rest = rest_ }, cmd_ )

        Msg (Types.Select id) ->
            let
                ( active_, rest_ ) =
                    partition id ( model.active, model.rest )

                ( active__, context_, cmd_ ) =
                    Block.update model.context gd msg active_
            in
            ( { model | active = active__, context = context_, rest = rest_ }, cmd_ )

        _ ->
            let
                ( active_, context_, cmd_ ) =
                    Block.update model.context gd msg model.active
            in
            ( { model | active = active_, context = context_ }, cmd_ )


addBlock : Block -> Group msg -> Group msg
addBlock block group =
    { group | rest = block :: group.rest }


clearSelection : Group msg -> Group msg
clearSelection group =
    let
        active_ =
            group.active
                |> Maybe.map Block.clearSelection
                |> MaybeEx.toList
    in
    { group | active = Nothing, rest = group.rest ++ active_ }


view : Grid -> Group msg -> List (Svg.Svg msg)
view gd group =
    let
        viewBlock =
            Block.view group.context gd

        idleBlocks =
            group.rest |> List.map viewBlock

        activeBlock =
            group.active |> Maybe.map viewBlock |> MaybeEx.toList
    in
    idleBlocks ++ activeBlock


partition :
    Types.Id
    -> ( Maybe Block, List Block )
    -> ( Maybe Block, List Block )
partition id ( active, rest ) =
    case active of
        Just (Block bd) ->
            if bd.key == id.key then
                ( active, rest )

            else
                partition id ( Nothing, rest ++ [ Block { bd | state = Types.Idle } ] )

        Nothing ->
            let
                ( matches, rest_ ) =
                    List.partition (\(Block bd) -> bd.key == id.key) rest

                match =
                    List.head matches
            in
            ( match, rest_ )
