module Table exposing (..)

import Utility


type alias Column a =
    List a


type alias Table a =
    List (Column a)


testTable =
    [ col1, col2 ]


col1 =
    [ 1, 2, 3 ]


col2 =
    [ 10, 20, 30 ]


row : Int -> Table a -> Maybe (List a)
row k table =
    List.map (Utility.listGetAt k) table |> Utility.maybeCombine


nRows : Table a -> Int
nRows table =
    List.length table


nCols : Table a -> Int
nCols table =
    Maybe.map List.length (List.head table) |> Maybe.withDefault 0


{-| getColumn i table extracts a list of Floats from
a table (List of Records) by extracting column i of the
Table, transforming the result to lists of floats.
-}
getColumn : Int -> Table a -> Maybe (List a)
getColumn k table =
    table
        |> List.map (Utility.listGetAt k)
        |> Utility.maybeCombine


{-| getColumn i table extracts a list of Floats from
a table (List of Records) by extracting column i of the
Table, transforming the result to lists of floats.
-}
getColumnAsFloats : Int -> Table String -> Maybe (List Float)
getColumnAsFloats k table =
    table
        |> List.map (Utility.listGetAt k)
        |> List.map (Maybe.andThen String.toFloat)
        |> Utility.maybeCombine
