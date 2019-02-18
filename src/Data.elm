module Data exposing (Point, Data)

{-| This module provides functions for
extracting `Data` both from strings and
`RawData` values.

@docs Point, Data

-}


{-| A point ni the xy plane
-}
type alias Point =
    { x : Float
    , y : Float
    }


{-| A list of points in the xy plane. Used for making graphs.
-}
type alias Data =
    List Point
