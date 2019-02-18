module Data exposing (get, fromString)

{-| This module provides functions for
extracting `Data` both from strings and
`RawData` values.

@docs fromString, get

-}

import Types exposing (Point, Data)
import RawData exposing (RawData)


{-| Examples:

    > SampleData.eb2 |> RawData.get
      Just { columnHeaders = ["x","y"]
          , data = [["0","1.0"],["0","0.9"],["1","1.8"],["0","1.0"],["1","2.0"]
                   ,["1","2.2"],["0","1.1"]]
          , metadata = [] }

-}
get : Int -> Int -> RawData -> Maybe Data
get i j rawData_ =
    let
        xs =
            rawData_.data |> RawData.getColumn i

        ys =
            rawData_.data |> RawData.getColumn j
    in
        case ( xs, ys ) of
            ( Just xss, Just yss ) ->
                Just (List.map2 Point xss yss)

            ( _, _ ) ->
                Nothing


{-| Example:

    > SampleData.eb2 |> Data.fromString 0 1
      [{ x = 0, y = 1 },{ x = 0, y = 0.9 },{ x = 1, y = 1.8 }
      ,{ x = 0, y = 1 },{ x = 1, y = 2 },{ x = 1, y = 2.2 }
      ,{ x = 0, y = 1.1 }]

-}
fromString : Int -> Int -> String -> Data
fromString i j str =
    str
        |> RawData.get
        |> Maybe.andThen (get i j)
        |> Maybe.withDefault []
