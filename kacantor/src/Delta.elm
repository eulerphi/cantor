module Delta exposing (..)


type alias Delta =
    { dx : Float
    , dy : Float
    }


init : ( Float, Float ) -> Delta
init ( dx, dy ) =
    { dx = dx, dy = dy }


addX : Delta -> Delta -> Delta
addX d1 d2 =
    { dx = d1.dx + d2.dx
    , dy = d1.dy
    }
