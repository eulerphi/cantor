module MaybeEx exposing (..)


filter : (a -> Bool) -> Maybe a -> Maybe a
filter fn maybe =
    let
        filterFn =
            \val ->
                if fn val then
                    maybe

                else
                    Nothing
    in
    maybe |> Maybe.andThen filterFn


equals : a -> Maybe a -> Bool
equals target maybe =
    maybe
        |> Maybe.map (\val -> val == target)
        |> Maybe.withDefault False


toMappedList : (a -> b) -> Maybe a -> List b
toMappedList fn maybe =
    case maybe of
        Nothing ->
            []

        Just value ->
            [ fn value ]


toList : Maybe a -> List a
toList maybe =
    case maybe of
        Nothing ->
            []

        Just value ->
            [ value ]
