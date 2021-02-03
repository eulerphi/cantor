module Delta exposing (..)


type alias Delta =
    { dx : Float
    , dy : Float
    }


init : ( Float, Float ) -> Delta
init ( dx, dy ) =
    { dx = dx, dy = dy }
