module Stat
    exposing
        ( Statistics
        , mean
        , filter
        , maximum
        , minimum
        , mode
        , statistics
        , stdev
        )

{-| The aim of this library is to compute statistics for 2D data.

@docs Statistics, mean, mode, filter, maximum, minimum, statistics, stdev

-}

import Data exposing (Point, Data, xCoord, yCoord)
import Dict exposing (Dict)


{-| A `Statistics` value holds information like
the mean and standard deviation of the x and y
values of `Data` value (list of points), as well
as the coefficients `m` and `b` of the regression
line, the `R^2` value, etc. Compute using
`statistics data`.
-}
type alias Statistics =
    { m : Float
    , b : Float
    , n : Int
    , r2 : Float
    , xMin : Float
    , xMax : Float
    , xMean : Float
    , yMean : Float
    , xStdev : Float
    , yStdev : Float
    , leftDataPoint : Point
    , rightDataPoint : Point
    , leftRegressionPoint : Point
    , rightRegressionPoint : Point
    }


{-| A `Filter` value contains the information
needed to apply a filter to the data, e.g.,
restrict the range of the x-values.
-}
type alias Filter =
    { xMin : Maybe Float
    , xMax : Maybe Float
    }


{-| Apply filters to the data, This is set
up as a pipeline so that other filters
can be added later. We may have to go
with different architectue if we have
may filters -- best to avoid too many
repeated list traversals.
-}
filter : Filter -> Data -> Data
filter filter_ data =
    data
        |> restrictXRange filter_


{-| Reststrict the range of x-values.
Note that the filter is the identity map
if either of xMin or yMin has value Nothing.
-}
restrictXRange : Filter -> Data -> Data
restrictXRange filter_ data =
    case ( filter_.xMin, filter_.xMax ) of
        ( Just xMin, Just xMax ) ->
            List.filter (\point -> xCoord point >= xMin && xCoord point <= xMax) data

        ( _, _ ) ->
            data


{-| Compute the statistics of a `Data` value.
-}
statistics : Data -> Maybe Statistics
statistics data =
    let
        nn =
            List.length data
    in
        case nn < 2 of
            True ->
                Nothing

            False ->
                let
                    n =
                        toFloat nn

                    xs =
                        List.map xCoord data

                    ys =
                        List.map yCoord data

                    xMin =
                        minimum xCoord data |> Maybe.withDefault 0

                    xMax =
                        maximum xCoord data |> Maybe.withDefault 0

                    origin =
                        ( 0, 0 )

                    leftDataPoint =
                        data |> List.filter (\point -> xCoord point == xMin) |> List.head |> Maybe.withDefault origin

                    rightDataPoint =
                        data |> List.filter (\point -> xCoord point == xMax) |> List.head |> Maybe.withDefault origin

                    xSum =
                        List.sum xs

                    xMean =
                        xSum / n

                    ySum =
                        List.sum ys

                    yMean =
                        ySum / n

                    xsSquared =
                        List.sum (List.map (\x -> x * x) xs)

                    xySum =
                        List.map2 (*) xs ys |> List.sum

                    square x =
                        x * x

                    ssTot =
                        List.sum (List.map (\y -> square (y - yMean)) ys)

                    xDeltaSquaredSum =
                        xs |> List.map (\x -> square (x - xMean)) |> List.sum

                    yDeltaSquaredSum =
                        ys |> List.map (\y -> square (y - yMean)) |> List.sum

                    xStdev =
                        sqrt (xDeltaSquaredSum / (n - 1))

                    yStdev =
                        sqrt (yDeltaSquaredSum / (n - 1))

                    determinant =
                        n * xDeltaSquaredSum

                    m =
                        (1 / determinant) * (n * xySum - xSum * ySum)

                    b =
                        (1 / determinant) * (-xSum * xySum + xsSquared * ySum)

                    fs =
                        List.map (\x -> m * x + b) xs

                    ssRes =
                        List.sum (List.map2 (\f y -> square (f - y)) fs ys)

                    r2 =
                        1 - ssRes / ssTot

                    leftRegressionPoint =
                        ( xCoord leftDataPoint, m * (xCoord leftDataPoint) + b )

                    rightRegressionPoint =
                        ( xCoord rightDataPoint, m * (xCoord rightDataPoint) + b )
                in
                    Just
                        { n = nn
                        , xMax = xMax
                        , xMin = xMin
                        , xMean = xMean
                        , yMean = yMean
                        , xStdev = xStdev
                        , yStdev = yStdev
                        , m = m
                        , b = b
                        , r2 = r2
                        , leftDataPoint = leftDataPoint
                        , rightDataPoint = rightDataPoint
                        , leftRegressionPoint = leftRegressionPoint
                        , rightRegressionPoint = rightRegressionPoint
                        }


{-| Compute the mean of a column in a list of data, e.g.,

    mean xCoord data

which computes the mean of the x-values.

-}
mean : (data -> Float) -> List data -> Maybe Float
mean selector dataList =
    let
        values =
            List.map selector dataList

        sum =
            List.sum values

        n =
            toFloat (List.length values)
    in
        case n > 0 of
            True ->
                Just <| sum / n

            False ->
                Nothing


{-| Compute the standard deviation of a column in a list of data, e.g.,

    stdev xCoord data

which computes the standard deviation of the x-values.

-}
stdev : (data -> Float) -> List data -> Maybe Float
stdev selector dataList =
    let
        n =
            List.length dataList
    in
        case n > 1 of
            False ->
                Nothing

            True ->
                let
                    mean_ =
                        mean selector dataList |> Maybe.withDefault 0

                    square x =
                        x * x

                    squaredDifferences =
                        List.map (\x -> square (x - mean_)) (List.map selector dataList)
                in
                    Just <| List.sum squaredDifferences / toFloat (n - 1)


{-| Compute the minimum of a column in a list of data, e.g.,

    minimum xCoord data

which computes the minimum of the x-values.

-}
minimum : (data -> Float) -> List data -> Maybe Float
minimum selector dataList =
    List.minimum (List.map selector dataList)


{-| Compute the maximum of a column in a list of data, e.g.,

    maximum xCoord data

which computes the maximum of the x-values.

-}
maximum : (data -> Float) -> List data -> Maybe Float
maximum selector dataList =
    List.maximum (List.map selector dataList)


type alias FrequencyTable comparable =
    Dict comparable Int


addToTable : comparable -> FrequencyTable comparable -> FrequencyTable comparable
addToTable item dict =
    case Dict.get item dict of
        Nothing ->
            Dict.insert item 1 dict

        Just f ->
            Dict.insert item (f + 1) dict


{-| buildTable [1,2,3,3,1,1,2,1] == Dict.fromList [(1,4),(2,2),(3,2)]
-}
buildTable : List comparable -> FrequencyTable comparable
buildTable list =
    list |> List.foldl (\item dict -> addToTable item dict) Dict.empty


{-| Compute the mode of the data:

    > data = [1, 5, 2, 2, 2, 2, 5, 3, 1]
    > mode data
      Just (2,4) : Maybe ( number, Int )

    > data = ["red", "green", "red", "blue", "blue", "red"]
    > mode data
      Just ("red",3) : Maybe ( String, Int )

-}
mode : List comparable -> Maybe ( comparable, Int )
mode list =
    let
        frequencyTable =
            buildTable list

        maxValue =
            List.maximum (Dict.values frequencyTable)

        kvList =
            Dict.toList frequencyTable
    in
        List.filter (\( k, v ) -> Just v == maxValue) kvList
            |> List.head
            |> Maybe.map Tuple.first
            |> (\x -> ( x, maxValue ))
            |> combineTuple


combineTuple : ( Maybe a, Maybe b ) -> Maybe ( a, b )
combineTuple ( aa, bb ) =
    case ( aa, bb ) of
        ( Just k, Just v ) ->
            Just ( k, v )

        ( _, _ ) ->
            Nothing
