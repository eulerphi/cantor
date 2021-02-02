module Pair exposing (..)


add : ( number, number ) -> ( number, number ) -> ( number, number )
add p1 p2 =
    ( Tuple.first p1 + Tuple.first p2
    , Tuple.second p1 + Tuple.second p2
    )


addFirst : ( number, number ) -> ( number, number ) -> ( number, number )
addFirst p1 p2 =
    ( Tuple.first p1 + Tuple.first p2
    , Tuple.second p1
    )


addSecond : ( number, number ) -> ( number, number ) -> ( number, number )
addSecond p1 p2 =
    ( Tuple.first p1
    , Tuple.second p1 + Tuple.second p2
    )


map : (a -> b) -> ( a, a ) -> ( b, b )
map fn pair =
    Tuple.mapBoth fn fn pair
