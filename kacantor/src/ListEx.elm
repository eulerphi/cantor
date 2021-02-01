module ListEx exposing (..)


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        [ item ] ->
            Just item

        _ :: rest ->
            last rest
