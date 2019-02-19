module Data exposing (Point, Data, BarePoint, BareData, toBarePoint, toBareData)

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


type alias BarePoint =
    ( Float, Float )


type alias BareData =
    List BarePoint


toBarePoint : Point -> BarePoint
toBarePoint p =
    ( p.x, p.y )


toBareData : Data -> BareData
toBareData data =
    List.map toBarePoint data
