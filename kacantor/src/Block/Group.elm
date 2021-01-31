module Block.Group exposing (..)

import Block


update :
    Block.Config msg
    -> Block.Msg
    -> Block.Group
    -> ( Block.Group, Cmd msg )
update config msg model =
    case msg of
        Block.StartDrag id ->
            let
                ( matches, rest ) =
                    List.partition (\bd -> bd.id == Tuple.first id) model.idle

                active =
                    List.head matches

                ( active_, dragState_, cmd_ ) =
                    Block.update config msg model.dragState active
            in
            ( { model | drag = active_, dragState = dragState_, idle = rest }, cmd_ )

        Block.EndDrag ->
            let
                ( active_, dragState_, cmd_ ) =
                    Block.update config msg model.dragState model.drag

                rest_ =
                    active_
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                        |> (++) model.idle
            in
            ( { model | drag = Nothing, dragState = dragState_, idle = rest_ }, cmd_ )

        _ ->
            let
                ( active_, dragState_, cmd_ ) =
                    Block.update config msg model.dragState model.drag
            in
            ( { model | drag = active_, dragState = dragState_ }, cmd_ )



-- startDragging : String -> Block.Group -> Block.Group
-- startDragging id group =
--     let
--         ( drags, idles ) =
--             List.partition (\bd -> bd.id == id) group.idle
--         drag =
--             List.head drags |> Maybe.map Block.startDrag
--     in
--     { idle = idles
--     , drag = drag
--     , dragState
--     }
-- endDragging : Grid.Data -> Block.Group -> Block.Group
-- endDragging gd group =
--     let
--         drag =
--             group.drag
--                 |> Maybe.map (Block.endDrag gd)
--                 |> Maybe.map (\bd -> [ bd ])
--                 |> Maybe.withDefault []
--     in
--     { idle = group.idle ++ drag
--     , drag = Nothing
--     }
-- onDrag : ( Int, Int ) -> Block.Group -> Block.Group
-- onDrag delta group =
--     { group | drag = Maybe.map (Block.onDrag delta) group.drag }
