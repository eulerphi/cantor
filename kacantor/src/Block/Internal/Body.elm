module Block.Internal.Body exposing (..)

import Block.Model exposing (..)
import Grid
import Pair
import Svg
import Svg.Attributes as SvgAttrs


updateDragMoveDelta : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
updateDragMoveDelta oldDelta newDelta =
    Pair.add oldDelta newDelta
