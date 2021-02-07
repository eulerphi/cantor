module Pos exposing (..)

import Delta exposing (Delta)
import MathEx


type alias Pos =
    { x : Float
    , y : Float
    }


fromInt : ( Int, Int ) -> Pos
fromInt ( x, y ) =
    { x = toFloat x, y = toFloat y }


init : ( Float, Float ) -> Pos
init ( x, y ) =
    { x = x, y = y }


origin : Pos
origin =
    init ( 0, 0 )


add : Pos -> Pos -> Pos
add p1 p2 =
    { x = p1.x + p2.x
    , y = p1.y + p2.y
    }


add3 : Pos -> Pos -> Pos -> Pos
add3 p1 p2 p3 =
    { x = p1.x + p2.x + p3.x
    , y = p1.y + p2.y + p3.y
    }


add4 : Pos -> Pos -> Pos -> Pos -> Pos
add4 p1 p2 p3 p4 =
    { x = p1.x + p2.x + p3.x + p4.x
    , y = p1.y + p2.y + p3.y + p4.y
    }


addX : Pos -> Pos -> Pos
addX p1 p2 =
    { x = p1.x + p2.x
    , y = p1.y
    }


addY : Pos -> Pos -> Pos
addY p1 p2 =
    { x = p1.x
    , y = p1.y + p2.y
    }


addDelta : Delta -> Pos -> Pos
addDelta delta p1 =
    { x = p1.x + delta.dx
    , y = p1.y + delta.dy
    }


addDeltaX : Pos -> Delta -> Pos
addDeltaX p1 delta =
    { x = p1.x + delta.dx
    , y = p1.y
    }


addDeltaY : Pos -> Delta -> Pos
addDeltaY p1 delta =
    { x = p1.x
    , y = p1.y + delta.dy
    }


deltaBetween : Pos -> Pos -> Delta
deltaBetween start end =
    Delta (end.x - start.x) (end.y - start.y)


map : (Float -> Float) -> Pos -> Pos
map fn pos =
    { x = fn pos.x, y = fn pos.y }


roundNear :
    { pos : Pos
    , unit : Float
    }
    -> Pos
    -> Pos
roundNear input pos =
    { x = input.pos.x + MathEx.roundNear input.unit (pos.x - input.pos.x)
    , y = input.pos.y + MathEx.roundNear input.unit (pos.y - input.pos.y)
    }


scale : Float -> Pos -> Pos
scale unit pos =
    { x = unit * pos.x, y = unit * pos.y }


scaleByInt : Int -> Pos -> Pos
scaleByInt unit pos =
    scale (toFloat unit) pos


toXString : Pos -> String
toXString pos =
    String.fromFloat pos.x


toYString : Pos -> String
toYString pos =
    String.fromFloat pos.y
