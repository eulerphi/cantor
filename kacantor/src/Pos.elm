module Pos exposing (..)


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Delta =
    { dx : Float
    , dy : Float
    }


add : Pos -> Pos -> Pos
add p1 p2 =
    { x = p1.x + p2.x
    , y = p1.y + p2.y
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


map : (Float -> Float) -> Pos -> Pos
map fn pos =
    { x = fn pos.x, y = fn pos.y }


addDelta : Pos -> Delta -> Pos
addDelta p1 delta =
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
