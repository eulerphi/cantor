module Block.Group exposing (..)

import Block
import Grid


startDragging : String -> Block.Group -> Block.Group
startDragging id group =
    let
        ( drags, idles ) =
            List.partition (\bd -> bd.id == id) group.idle

        drag =
            List.head drags |> Maybe.map Block.startDrag
    in
    { idle = idles
    , drag = drag
    }


endDragging : Grid.Data -> Block.Group -> Block.Group
endDragging gd group =
    let
        drag =
            group.drag
                |> Maybe.map (Block.endDrag gd)
                |> Maybe.map (\bd -> [ bd ])
                |> Maybe.withDefault []
    in
    { idle = group.idle ++ drag
    , drag = Nothing
    }


onDrag : ( Int, Int ) -> Block.Group -> Block.Group
onDrag delta group =
    { group | drag = Maybe.map (Block.onDrag delta) group.drag }
