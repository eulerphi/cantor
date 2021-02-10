module Block.Internal.Config exposing
    ( barLineWidth
    , circleActiveSize
    , circleIdleSize
    , connectorLineWidth
    , guideLineWidth
    , offsetPosDelta
    , outlinePosDelta
    , outlineSizeDelta
    )

import Delta exposing (Delta)
import Size exposing (Size)


barLineWidth : Float
barLineWidth =
    3


guideLineWidth : Float
guideLineWidth =
    3


circleActiveSize : Float -> Float
circleActiveSize unit =
    unit / 1.2


circleIdleSize : Float -> Float
circleIdleSize unit =
    unit / 3


connectorLineWidth : Float
connectorLineWidth =
    3


offsetPosDelta : Delta
offsetPosDelta =
    let
        { dx } =
            outlinePosDelta
    in
    Delta dx 0


outlinePosDelta : Delta
outlinePosDelta =
    Delta -4 -4


outlineSizeDelta : Size
outlineSizeDelta =
    let
        { dx, dy } =
            outlinePosDelta |> Delta.scale -2
    in
    Size dx dy
