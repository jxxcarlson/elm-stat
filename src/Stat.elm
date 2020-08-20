module Stat exposing
    ( mean, meanWithDefault, average, geometricMean, harmonicMean, weightedMean, median, mode, rootMeanSquare, skewness
    , variance, standardDeviation, meanAbsoluteDeviation, medianAbsoluteDeviation, zScore, zScores
    , covariance, correlation, r2
    , linearRegression, linearRegressionLine
    )

{-| The goal of this module is to provide the most comonly used statistical functions .


# Measures of Central Tendency

@docs mean, meanWithDefault, average, geometricMean, harmonicMean, weightedMean, median, mode, rootMeanSquare, skewness


# Measures of Dispersion

@docs variance, standardDeviation, meanAbsoluteDeviation, medianAbsoluteDeviation, zScore, zScores


# Similarity

@docs covariance, correlation, r2


# Linear Regression

@docs linearRegression, linearRegressionLine

-}

import Dict as Dict
import Utility exposing (buildTable, combineTuple)


{-| Compute the mean of a list of floats.

    > Stat.mean [1,2,4,5] == Just 3
    > Stat.mean [] == Nothing

-}
mean : List Float -> Maybe Float
mean list =
    let
        ( sum, len ) =
            sumLen list
    in
    if len == 0 then
        Nothing

    else
        sum / toFloat len |> Just


sumLen : List Float -> ( Float, Int )
sumLen =
    List.foldl (\x y -> Tuple.pair (Tuple.first y + x) (Tuple.second y + 1)) ( 0, 0 )


{-| Same as mean

    > Stat.average [1,2,4,5] == Just 3
    > Stat.average [] == Nothing

-}
average : List Float -> Maybe Float
average =
    mean


{-| Compute the mean of a list of floats, but in case of an empty list return the default value that was provided.

    > Stat.meanWithDefault [1,2,4,5] 0 == 3
    > Stat.meanWithDefault [] 0 == 0

-}
meanWithDefault : List Float -> Float -> Float
meanWithDefault list defaultValue =
    let
        avg =
            mean list
    in
    case avg of
        Nothing ->
            defaultValue

        Just val ->
            val


{-| Compute the weighted mean of a list of tuples, where the first elemnt in the tuple is the weight and the second is the value

    > Stat.weightedMean [(2,5),(8,10)] == Just 9
    > Stat.weightedMean [(0,5),(0,10)] == Nothing -- the sum of the weights can not be 0

-}
weightedMean : List ( Float, Float ) -> Maybe Float
weightedMean tupleList =
    let
        wSum =
            List.map (\t -> Tuple.first t) tupleList |> List.sum
    in
    if wSum == 0 then
        Nothing

    else
        (List.map (\t -> Tuple.first t * Tuple.second t) tupleList |> List.sum) / wSum |> Just


{-| Compute the harmonic mean of a list of floats.

    > Stat.harmonicMean [1,2,4,5] == Just 2.0512820512820515

-}
harmonicMean : List Float -> Maybe Float
harmonicMean list =
    let
        sum =
            List.sum (List.map (\x -> x ^ -1) list)
    in
    if sum == 0 then
        Nothing

    else
        toFloat (List.length list) / sum |> Just


{-| Compute the geometric mean of a list of floats.

    > Stat.geometricMean [1,2.7,5.9] == Just 2.51

-}
geometricMean : List Float -> Maybe Float
geometricMean list =
    let
        l =
            List.length list
    in
    if l == 0 then
        Nothing

    else
        List.product list ^ (1 / toFloat l) |> Just


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
    List.filter (\( _, v ) -> Just v == maxValue) kvList
        |> List.head
        |> Maybe.map Tuple.first
        |> (\x -> ( x, maxValue ))
        |> combineTuple


{-| Compute the median of the list. The median is the value separating the higher half from the lower half of a data sample. If the sample has an odd number of values, the median is the value in the middle. If the sample has an even number of values, the median is the mean of the two middle values.

    > Stat.median [1,6,10] == Just 6
    > Stat.median [1,6,8,10] == Just 7

-}
median : List Float -> Maybe Float
median list =
    let
        l =
            List.length list
    in
    if l == 0 then
        Nothing

    else if modBy 2 l == 0 then
        List.sort list
            |> List.drop ((l // 2) - 1)
            |> List.take 2
            |> mean

    else
        List.sort list
            |> List.drop (l // 2)
            |> List.head


{-| Root mean square (RMS) is the square root of the sum of the squares of values in a list divided by the length of the list. Also known as quadratic mean.

    Stat.rootMeanSquare [ 1, 10, 20 ] == Just 12.92

-}
rootMeanSquare : List Float -> Maybe Float
rootMeanSquare list =
    let
        l =
            List.length list
    in
    if l == 0 then
        Nothing

    else
        List.map (\x -> x ^ 2) list
            |> List.sum
            |> (\x -> x / toFloat l)
            |> sqrt
            |> Just


{-| Skew or Skewness is a measure of the asymmetry of the probability distribution of a variable around its mean. There are several equations to calculate skewness. The one used in this function is [Pearson’s moment coefficient](https://en.wikipedia.org/wiki/Pearson_correlation_coefficient) of skewness.

    > Stat.skewness [1,10.5,20] == Just 0
    > Stat.skewness [1,2,3,10] == Just 1.01
    > Stat.skewness [1,30,30,30] == Just -1.15

-}
skewness : List Float -> Maybe Float
skewness list =
    let
        stdDev =
            standardDeviation list
    in
    case stdDev of
        Nothing ->
            Nothing

        Just s ->
            mean list
                |> Maybe.andThen
                    (\n ->
                        List.map (\x -> ((x - n) / s) ^ 3) list
                            |> mean
                    )


{-| In statistics, variance is the expectation of the squared deviation of a random variable from its mean.

    > Stat.variance [1,2,3,4,5] == Just 2

-}
variance : List Float -> Maybe Float
variance list =
    mean list
        |> Maybe.andThen
            (\n ->
                List.map (\x -> (x - n) ^ 2) list
                    |> mean
            )


{-| The standard deviation is the square root of variance. A low standard deviation indicates that the values tend to be close to the mean.

    > Stat.standardDeviation [1,2,3,4,5] == Just 1.41
    > Stat.standardDeviation [2,2,2] == Just 0

-}
standardDeviation : List Float -> Maybe Float
standardDeviation =
    variance >> Maybe.map sqrt


{-| The average absolute deviation, or mean absolute deviation, of a data set is the average of the absolute deviations from the mean.

    > Stat.meanAbsoluteDeviation [1,2,5,4] == Just 1.5
    > Stat.meanAbsoluteDeviation [1,2,4] == Just 1.11

-}
meanAbsoluteDeviation : List Float -> Maybe Float
meanAbsoluteDeviation list =
    mean list
        |> Maybe.andThen
            (\n ->
                List.map (\x -> abs (x - n)) list |> mean
            )


{-| The median absolute deviation, of a data set is the average of the absolute deviations from the median.

    > Stat.medianAbsoluteDeviation [ 1, 2, 4 ] == Just 1

-}
medianAbsoluteDeviation : List Float -> Maybe Float
medianAbsoluteDeviation list =
    median list
        |> Maybe.andThen
            (\n ->
                List.map (\x -> abs (x - n)) list |> mean
            )


{-| Calculate the Z-score or standard score of a given elements provided the mean and the standard deviation.

    > Stat.zScore 1 3 1.58 == -1.26

-}
zScore : Float -> Float -> Float -> Float
zScore x avg stdDev =
    (x - avg) / stdDev


{-| Calculate the Z-score or standard score of the provided list

    > Stat.zScores [1,2,4,5] == Just [-1.26,-0.63,0.63,1.26]

-}
zScores : List Float -> Maybe (List Float)
zScores list =
    Maybe.map2 (\avg stdDev -> List.map (\x -> (x - avg) / stdDev) list) (mean list) (standardDeviation list)


{-| Covariance is a measure of how two random variables vary together. When the greater values of one variable correspond to the greater values of the other variable, this is a positive covariance. Whereas when the greater values of one variable correspond to the lesser values of the other variable, this is negative covariance.

    > Stat.covariance[(1,2),(4,8),(5,10)] == Just 5.77

-}
covariance : List ( Float, Float ) -> Maybe Float
covariance tupleList =
    let
        ( xs, ys ) =
            List.unzip tupleList
    in
    Maybe.map2 (\mxs mys -> List.map2 (\x y -> (x - mxs) * (y - mys)) xs ys) (mean xs) (mean ys)
        |> Maybe.andThen mean


{-| A correlation is a “normalized” covariance, its values are between -1.0 and 1.0

    > Stat.correlation[(1,2),(4,8),(5,10)] == Just 1.00
    > Stat.correlation[(10,0),(40,-30),(50,-32)] == Just -0.98

-}
correlation : List ( Float, Float ) -> Maybe Float
correlation tupleList =
    let
        ( xs, ys ) =
            List.unzip tupleList
    in
    Maybe.map3 (\cov stdDevXs stdDevYs -> cov / (stdDevXs * stdDevYs)) (covariance tupleList) (standardDeviation xs) (standardDeviation ys)


{-| R2 is the square of the correlation coefficient

    > Stat.r2[(1,2),(4,8),(5,10)] == Just 1.00
    > Stat.r2[(10,0),(40,-30),(50,-32)] == Just 0.97

-}
r2 : List ( Float, Float ) -> Maybe Float
r2 tupleList =
    let
        cc =
            correlation tupleList
    in
    Maybe.map2 (*) cc cc


{-| Linear regression finds the line that best fits the given points. The method used here is the [simple linear regression](https://en.wikipedia.org/wiki/Linear_regression).
The tuple returned is `(alpha, beta)` where `y = alpha + beta * x`

    > Stat.linearRegression[(1,3),(4,9),(5,11)] == Just (1,2) -- 3 = 1 + 2 * 1

-}
linearRegression : List ( Float, Float ) -> Maybe ( Float, Float )
linearRegression tupleList =
    if List.length tupleList > 1 then
        let
            ( xs, ys ) =
                List.unzip tupleList

            cov =
                covariance tupleList |> Maybe.withDefault 0

            varxs =
                variance xs |> Maybe.withDefault 0
        in
        if varxs /= 0 then
            Maybe.map2 (\mxs mys -> Tuple.pair (mys - (cov / varxs) * mxs) (cov / varxs)) (mean xs) (mean ys)

        else
            Nothing

    else
        Nothing


{-| Returns a function that looks like this: `y = alpha + beta * x`.
This may come in handy when generating points on the regreesion line.

    > Stat.linearRegression[(1,3),(4,9),(5,11)] == Just (1,2)
    > f = Stat.linearRegressionLine (1,2) == <function> : Float -> Float
    > f 5 == 11

-}
linearRegressionLine : ( Float, Float ) -> Float -> Float
linearRegressionLine t =
    lineFunc (Tuple.first t) (Tuple.second t)


lineFunc : Float -> Float -> Float -> Float
lineFunc alpha beta x =
    alpha + beta * x
