module Data exposing (getData, fromString)

{-| getData i j rawData_ extracts Data from
RawData by extracting columns i and j of the
RawData, trasforming these to lists of floats,
and using these as the x and y coordinates of
a list of Points.
-}

import Types exposing (Point, Data)
import RawData exposing (RawData)


getData : Int -> Int -> RawData -> Maybe Data
getData i j rawData_ =
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


fromString : Int -> Int -> String -> Data
fromString i j str =
    str
        |> RawData.get
        |> Maybe.andThen (getData i j)
        |> Maybe.withDefault []
