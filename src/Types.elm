module Types exposing (Point, Data)

{-| A point in the Cartesian plane
given by x and y coordinates
-}


type alias Point =
    { x : Float
    , y : Float
    }


{-| The fundamental data structure: a list of points.
-}
type alias Data =
    List Point
