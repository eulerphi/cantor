module Block.Internal.ViewModel exposing (..)

import Block.Internal.Section as Section exposing (Section, Section2, Sections)
import Block.Internal.Types exposing (..)
import Box exposing (Box)
import Grid exposing (Grid)
import Pos exposing (Pos)
import Size exposing (IntSize, Size)


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
            toBox gd bd

        sections =
            Section.forBlock2 gd bd
    in
    { pos = box.pos
    , size = box.size
    , grid = gd
    , block = bd
    , sections = sections
    }


toBox : Grid -> Block -> Box
toBox gd bd =
    let
        w =
            bd.product.width

        h =
            if bd.remainder > 0 then
                bd.product.height + 1

            else
                bd.product.height

        size =
            IntSize w h
                |> Size.toFloat
                |> Size.scale gd.unit
    in
    Box bd.pos size
