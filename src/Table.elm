module Table exposing (..)

import Utility


type alias Column a =
    List a


type alias Table a =
    List (Column a)


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
