module Size exposing (..)

import Pair


type alias Size =
    { width : Float
    , height : Float
    }


type alias IntSize =
    { width : Int
    , height : Int
    }


type alias Sizelike r number =
    { r
        | width : number
        , height : number
    }


floor : Size -> IntSize
floor size =
    size
        |> toPair
        |> Pair.map Basics.floor
        |> Pair.uncurry IntSize


toFloat : IntSize -> Size
toFloat size =
    size
        |> toPair
        |> Pair.map Basics.toFloat
        |> Pair.uncurry Size


inUnits : Float -> Size -> IntSize
inUnits unit size =
    size
        |> div unit
        |> floor


fromInt : ( Int, Int ) -> Size
fromInt ( w, h ) =
    { width = Basics.toFloat w, height = Basics.toFloat h }


init : ( Float, Float ) -> Size
init ( w, h ) =
    { width = w, height = h }


forSquare : Float -> Size
forSquare value =
    Size value value


none : Size
none =
    init ( 0, 0 )


noneInt : IntSize
noneInt =
    IntSize 0 0


add : Size -> Size -> Size
add s1 s2 =
    { width = s1.width + s2.width
    , height = s1.height + s2.height
    }


area : Sizelike r number -> number
area size =
    size.width * size.height


sub : Size -> Size -> Size
sub s1 s2 =
    Size (s1.width - s2.width) (s1.height - s2.height)


div : Float -> Size -> Size
div divisor size =
    Size (size.width / divisor) (size.height / divisor)


addHeight : Float -> Size -> Size
addHeight heightValue size =
    Size size.width (size.height + heightValue)


addWidth : Float -> Size -> Size
addWidth widthValue size =
    Size (size.width + widthValue) size.height



-- addWidth : Size -> Size -> Float
-- addWidth s1 s2 =
--     s1.width + s2.width
-- addHeight : Size -> Size -> Float
-- addHeight s1 s2 =
--     s1.height + s2.height
-- addHeight3 : Size -> Size -> Size -> Float
-- addHeight3 s1 s2 s3 =
--     s1.height + s2.height + s3.height


map : (Float -> Float) -> Size -> Size
map fn size =
    { width = fn size.width, height = fn size.height }


minDimension : Size -> Float
minDimension size =
    min size.width size.height


toPair : Sizelike r number -> ( number, number )
toPair size =
    ( size.width, size.height )


maxWidth3 : Size -> Size -> Size -> Float
maxWidth3 s1 s2 s3 =
    max s1.width (max s2.width s3.width)


scale : Float -> Size -> Size
scale unit size =
    { width = size.width * unit, height = size.height * unit }


scaleByInt : Int -> Size -> Size
scaleByInt unit size =
    scale (Basics.toFloat unit) size


toWidthString : Size -> String
toWidthString size =
    String.fromFloat size.width


toHeightString : Size -> String
toHeightString size =
    String.fromFloat size.height
