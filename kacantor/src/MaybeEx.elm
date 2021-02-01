module MaybeEx exposing (..)


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
