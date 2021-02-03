module Size exposing (..)


type alias Size =
    { width : Float
    , height : Float
    }


fromInt : ( Int, Int ) -> Size
fromInt ( w, h ) =
    { width = toFloat w, height = toFloat h }


init : ( Float, Float ) -> Size
init ( w, h ) =
    { width = w, height = h }


none : Size
none =
    init ( 0, 0 )


add : Size -> Size -> Size
add s1 s2 =
    { width = s1.width + s2.width
    , height = s1.height + s2.height
    }


add3 : Size -> Size -> Size -> Size
add3 s1 s2 s3 =
    { width = s1.width + s2.width + s3.width
    , height = s1.height + s2.height + s3.height
    }


addWidth : Size -> Size -> Size
addWidth s1 s2 =
    { width = s1.width + s2.width
    , height = s1.height
    }


addHeight : Size -> Size -> Size
addHeight s1 s2 =
    { width = s1.width
    , height = s1.height + s2.height
    }


map : (Float -> Float) -> Size -> Size
map fn size =
    { width = fn size.width, height = fn size.height }


scale : Float -> Size -> Size
scale unit size =
    { width = size.width * unit, height = size.height * unit }


scaleByInt : Int -> Size -> Size
scaleByInt unit size =
    scale (toFloat unit) size
