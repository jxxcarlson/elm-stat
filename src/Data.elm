module Data exposing (Point, Data, makePoint, xCoord, yCoord)

{-| This module provides functions for
extracting `Data` both from strings and
`RawData` values.

@docs Point, Data, makePoint, xCoord, yCoord

-}


{-| A point in the xy plane
-}
type alias Point =
    ( Float, Float )


{-| Construct a point
-}
makePoint : Float -> Float -> Point
makePoint x y =
    ( x, y )


{-| Get the x-coordinate of a point
-}
xCoord : Point -> Float
xCoord ( x, y ) =
    x


{-| Get the y-coordinate of a point
-}
yCoord : Point -> Float
yCoord ( x, y ) =
    y


{-| A list of points in the xy plane.
-}
type alias Data =
    List ( Float, Float )
