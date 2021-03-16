module OffsetAnchor exposing (..)

import Box exposing (Boxlike)
import Pos exposing (Pos)


type HorizontalAnchor
    = Left
    | HCenter
    | Right
    | X Float


type VerticalAnchor
    = Top
    | VCenter
    | Bottom
    | Y Float


type OffsetAnchor
    = OffsetAnchor HorizontalAnchor VerticalAnchor


toPos : OffsetAnchor -> Boxlike r -> Pos
toPos (OffsetAnchor h v) box =
    Pos (toX h box) (toY v box)


toX : HorizontalAnchor -> Boxlike r -> Float
toX anchor box =
    case anchor of
        Left ->
            box.pos.x

        HCenter ->
            box.pos.x + box.size.width / 2

        Right ->
            box.pos.x + box.size.width

        X xValue ->
            xValue


toY : VerticalAnchor -> Boxlike r -> Float
toY anchor box =
    case anchor of
        Top ->
            box.pos.y

        VCenter ->
            box.pos.y + box.size.height / 2

        Bottom ->
            box.pos.y + box.size.height

        Y yValue ->
            yValue
