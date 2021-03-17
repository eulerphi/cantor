module Block.Internal.ViewModel exposing (..)

import Block.Internal.Section as Section exposing (Section, Section2, Sections)
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


type alias ViewModel2 =
    { pos : Pos
    , size : Size
    , grid : Grid
    , block : Block
    , sections : Sections
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


forBlock2 : Grid -> Block -> ViewModel2
forBlock2 gd bd =
    let
        box =
            Section.toBox2 gd bd

        sections =
            Section.forBlock2 gd bd
    in
    { pos = box.pos
    , size = box.size
    , grid = gd
    , block = bd
    , sections = sections
    }
