module Block.Internal.ViewModel exposing (..)

import Block.Internal.Section as Section exposing (Section)
import Block.Internal.Types exposing (..)
import Grid exposing (Grid)
import Pos exposing (Pos)
import Size exposing (Size)


type alias ViewModel =
    { pos : Pos
    , size : Size
    , grid : Grid
    , block : Block
    , sections : List Section
    , tempBodySections : List Section
    , tempChangeSections : List Section
    }


forBlock : Grid -> Block -> ViewModel
forBlock gd bd =
    let
        ( sections, temps ) =
            Section.forBlockFoo gd bd

        box =
            (sections ++ temps) |> Section.toBox gd bd
    in
    { pos = bd.pos
    , size = box.size
    , grid = gd
    , block = bd
    , sections = Section.forBlock gd bd
    , tempBodySections = sections
    , tempChangeSections = temps
    }
