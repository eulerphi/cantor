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


addWidth : Size -> Size -> Float
addWidth s1 s2 =
    s1.width + s2.width


addHeight : Size -> Size -> Float
addHeight s1 s2 =
    s1.height + s2.height


addHeight3 : Size -> Size -> Size -> Float
addHeight3 s1 s2 s3 =
    s1.height + s2.height + s3.height


map : (Float -> Float) -> Size -> Size
map fn size =
    { width = fn size.width, height = fn size.height }


maxWidth3 : Size -> Size -> Size -> Float
maxWidth3 s1 s2 s3 =
    max s1.width (max s2.width s3.width)


scale : Float -> Size -> Size
scale unit size =
    { width = size.width * unit, height = size.height * unit }


scaleByInt : Int -> Size -> Size
scaleByInt unit size =
    scale (toFloat unit) size


toWidthString : Size -> String
toWidthString size =
    String.fromFloat size.width


toHeightString : Size -> String
toHeightString size =
    String.fromFloat size.height
