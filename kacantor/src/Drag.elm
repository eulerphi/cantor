module Drag exposing (..)


type State
    = None
    | Dragging ( Int, Int )
