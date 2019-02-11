module Stat exposing (Data, Point, Statistics, average, filterData, maximum, minimum, statistics, stdev)

{- \ The aim of this library is to copmute statistics for 2D data, of type
   `Data = List Point`, where `Paint = { x : Float , y : Float }.  Thus, if
   data is of type `Data`, you would calculate the aveerage of the x-values
   as in `average .x data`.  To get the full packate of statisitcal information
   on such a value, use `statistis data`.  The result is a value of type
   `Statitiscs`, described in detail below. It is a record containting the
   mean and standard deviation of the x and y values, the coefficeints
   `m` and `b` (as in `y = mx + b` of the regression line, the `R^2` coefficient,
   etc.
   )


-}


type alias Point =
    { x : Float
    , y : Float
    }


type alias Data =
    List Point


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
filterData : Filter -> Data -> Data
filterData filter data =
    data
        |> restrictXRange filter


{-| Reststrict the range of x-values.
Note that the filter is the identity map
if either of xMin or yMin has value Nothing.
-}
restrictXRange : Filter -> Data -> Data
restrictXRange filter data =
    case ( filter.xMin, filter.xMax ) of
        ( Just xMin, Just xMax ) ->
            List.filter (\point -> point.x >= xMin && point.x <= xMax) data

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
                    List.map .x data

                ys =
                    List.map .y data

                xMin =
                    minimum .x data |> Maybe.withDefault 0

                xMax =
                    maximum .x data |> Maybe.withDefault 0

                origin =
                    Point 0 0

                leftDataPoint =
                    data |> List.filter (\point -> .x point == xMin) |> List.head |> Maybe.withDefault origin

                rightDataPoint =
                    data |> List.filter (\point -> .x point == xMax) |> List.head |> Maybe.withDefault origin

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
                    { x = leftDataPoint.x, y = m * leftDataPoint.x + b }

                rightRegressionPoint =
                    { x = rightDataPoint.x, y = m * rightDataPoint.x + b }
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


average : (data -> Float) -> List data -> Maybe Float
average selector dataList =
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
                mean =
                    average selector dataList |> Maybe.withDefault 0

                square x =
                    x * x

                squaredDifferences =
                    List.map (\x -> square (x - mean)) (List.map selector dataList)
            in
            Just <| List.sum squaredDifferences / toFloat (n - 1)


minimum : (data -> Float) -> List data -> Maybe Float
minimum selector dataList =
    List.minimum (List.map selector dataList)


maximum : (data -> Float) -> List data -> Maybe Float
maximum selector dataList =
    List.maximum (List.map selector dataList)
