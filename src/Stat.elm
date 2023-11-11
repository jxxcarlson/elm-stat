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


{-| Compute the mean of a list of floats.

    > Stat.mean [1,2,4,5] == Just 3
    > Stat.mean [] == Nothing

-}
mean : List Float -> Maybe Float
mean list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (meanHelp xs x 1)


meanHelp : List Float -> Float -> Float -> Float
meanHelp remaining total length =
    case remaining of
        [] ->
            total / length

        x :: rest ->
            meanHelp rest (total + x) (length + 1)


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
    case list of
        [] ->
            defaultValue

        x :: xs ->
            meanHelp xs x 1


{-| Compute the weighted mean of a list of tuples, where the first elemnt in the tuple is the weight and the second is the value

    > Stat.weightedMean [(2,5),(8,10)] == Just 9
    > Stat.weightedMean [(0,5),(0,10)] == Nothing -- the sum of the weights can not be 0

-}
weightedMean : List ( Float, Float ) -> Maybe Float
weightedMean tupleList =
    case tupleList of
        [] ->
            Nothing

        ( w, x ) :: xs ->
            weightedMeanHelp xs w (w * x)


weightedMeanHelp : List ( Float, Float ) -> Float -> Float -> Maybe Float
weightedMeanHelp remaining totalWeight weightedSum =
    case remaining of
        [] ->
            if totalWeight == 0 then
                Nothing

            else
                Just (weightedSum / totalWeight)

        ( w, x ) :: xs ->
            weightedMeanHelp xs (totalWeight + w) (weightedSum + w * x)


{-| Compute the harmonic mean of a list of floats.

    > Stat.harmonicMean [1,2,4,5] == Just 2.0512820512820515

-}
harmonicMean : List Float -> Maybe Float
harmonicMean list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            harmonicMeanHelp xs (1 / x) 1


harmonicMeanHelp : List Float -> Float -> Float -> Maybe Float
harmonicMeanHelp remaining inverseSum length =
    case remaining of
        [] ->
            if inverseSum == 0 then
                Nothing

            else
                Just (length / inverseSum)

        x :: xs ->
            harmonicMeanHelp xs (inverseSum + 1 / x) (length + 1)


{-| Compute the geometric mean of a list of floats.

    > Stat.geometricMean [1,2.7,5.9] == Just 2.51

-}
geometricMean : List Float -> Maybe Float
geometricMean list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (geometricMeanHelp xs x 1)


geometricMeanHelp : List Float -> Float -> Float -> Float
geometricMeanHelp remaining product length =
    case remaining of
        [] ->
            product ^ (1 / length)

        x :: xs ->
            geometricMeanHelp xs (product * x) (length + 1)


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
    case List.sort list of
        [] ->
            Nothing

        x :: xs ->
            Just (modeHelp1 xs x 1)


modeHelp1 : List a -> a -> number -> ( a, number )
modeHelp1 rest element frequency =
    case rest of
        [] ->
            ( element, frequency )

        x :: xs ->
            if x == element then
                modeHelp1 xs element (frequency + 1)

            else
                modeHelp2 xs element frequency x 1


modeHelp2 : List a -> a -> number -> a -> number -> ( a, number )
modeHelp2 rest best bestFreq new newFreq =
    case rest of
        [] ->
            if newFreq > bestFreq then
                ( new, newFreq )

            else
                ( best, bestFreq )

        x :: xs ->
            if x == new then
                modeHelp2 xs best bestFreq new (newFreq + 1)

            else if newFreq > bestFreq then
                modeHelp2 rest new newFreq x 1

            else
                modeHelp2 xs best bestFreq x 1


{-| Compute the median of the list. The median is the value separating the higher half from the lower half of a data sample. If the sample has an odd number of values, the median is the value in the middle. If the sample has an even number of values, the median is the mean of the two middle values.

    > Stat.median [1,6,10] == Just 6
    > Stat.median [1,6,8,10] == Just 7

-}
median : List Float -> Maybe Float
median list =
    case list of
        [] ->
            Nothing

        _ :: xs ->
            let
                length : number
                length =
                    lengthHelp xs 1
            in
            if modBy 2 length == 0 then
                case
                    List.sort list
                        |> List.drop ((length // 2) - 1)
                of
                    x :: y :: _ ->
                        Just <| (x + y) / 2

                    _ ->
                        Nothing

            else
                case
                    List.sort list
                        |> List.drop (length // 2)
                of
                    x :: _ ->
                        Just x

                    _ ->
                        Nothing


lengthHelp : List a -> number -> number
lengthHelp remaining total =
    case remaining of
        [] ->
            total

        _ :: xs ->
            lengthHelp xs (total + 1)


{-| Root mean square (RMS) is the square root of the sum of the squares of values in a list divided by the length of the list. Also known as quadratic mean.

    Stat.rootMeanSquare [ 1, 10, 20 ] == Just 12.92

-}
rootMeanSquare : List Float -> Maybe Float
rootMeanSquare list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (rootMeanSquareHelp xs (x ^ 2) 1)


rootMeanSquareHelp : List Float -> Float -> Float -> Float
rootMeanSquareHelp remaining total length =
    case remaining of
        [] ->
            sqrt (total / length)

        x :: xs ->
            rootMeanSquareHelp xs (total + x ^ 2) (length + 1)


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
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (varianceHelp xs (x ^ 2) x 1)


varianceHelp : List Float -> Float -> Float -> Float -> Float
varianceHelp remaining squaredSum sum length =
    case remaining of
        [] ->
            squaredSum / length - (sum / length) ^ 2

        x :: xs ->
            varianceHelp xs (squaredSum + x ^ 2) (sum + x) (length + 1)


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
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just <|
                let
                    avg : Float
                    avg =
                        meanHelp xs x 1
                in
                meanAbsoluteDeviationHelp xs avg (abs (avg - x)) 1


meanAbsoluteDeviationHelp : List Float -> Float -> Float -> Float -> Float
meanAbsoluteDeviationHelp remaining avg total length =
    case remaining of
        [] ->
            total / length

        x :: xs ->
            meanAbsoluteDeviationHelp xs avg (total + abs (avg - x)) (length + 1)


{-| The median absolute deviation, of a data set is the average of the absolute deviations from the median.

    > Stat.medianAbsoluteDeviation [ 1, 2, 4 ] == Just 1

-}
medianAbsoluteDeviation : List Float -> Maybe Float
medianAbsoluteDeviation list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            case median list of
                Just mdn ->
                    Just (meanAbsoluteDeviationHelp xs mdn (abs (mdn - x)) 1)

                _ ->
                    Nothing


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
    case list of
        [] ->
            Nothing

        head :: rest ->
            zScoresHelp1 rest (head ^ 2) head 1 [ head ]
                |> Just



-- The reversed argument is to take advantage of the reverse & map pattern in zScoresHelp2


zScoresHelp1 : List Float -> Float -> Float -> Float -> List Float -> List Float
zScoresHelp1 remaining squaredSum sum length reversed =
    case remaining of
        [] ->
            let
                avg : Float
                avg =
                    sum / length
            in
            zScoresHelp2 avg (sqrt (squaredSum / length - avg ^ 2)) reversed []

        x :: xs ->
            zScoresHelp1 xs (squaredSum + x ^ 2) (sum + x) (length + 1) (x :: reversed)


zScoresHelp2 : Float -> Float -> List Float -> List Float -> List Float
zScoresHelp2 avg stdDev remaining acc =
    case remaining of
        [] ->
            acc

        x :: xs ->
            zScoresHelp2 avg stdDev xs (((x - avg) / stdDev) :: acc)


{-| Covariance is a measure of how two random variables vary together. When the greater values of one variable correspond to the greater values of the other variable, this is a positive covariance. Whereas when the greater values of one variable correspond to the lesser values of the other variable, this is negative covariance.

    > Stat.covariance[(1,2),(4,8),(5,10)] == Just 5.77

-}
covariance : List ( Float, Float ) -> Maybe Float
covariance tupleList =
    case tupleList of
        [] ->
            Nothing

        ( a, b ) :: xs ->
            let
                { meanA, meanB, length } =
                    covarianceHelp1 xs a b 1
            in
            covarianceHelp2 xs meanA meanB ((a - meanA) * (b - meanB))
                / length
                |> Just


covarianceHelp1 :
    List ( Float, Float )
    -> Float
    -> Float
    -> Float
    -> { meanA : Float, meanB : Float, length : Float }
covarianceHelp1 remaining sumA sumB length =
    case remaining of
        [] ->
            { meanA = sumA / length
            , meanB = sumB / length
            , length = length
            }

        ( a, b ) :: xs ->
            covarianceHelp1 xs (sumA + a) (sumB + b) (length + 1)


covarianceHelp2 : List ( Float, Float ) -> Float -> Float -> Float -> Float
covarianceHelp2 remaining meanA meanB acc =
    case remaining of
        [] ->
            acc

        ( a, b ) :: xs ->
            covarianceHelp2 xs meanA meanB (acc + (a - meanA) * (b - meanB))


{-| A correlation is a “normalized” covariance, its values are between -1.0 and 1.0

    > Stat.correlation[(1,2),(4,8),(5,10)] == Just 1.00
    > Stat.correlation[(10,0),(40,-30),(50,-32)] == Just -0.98

-}
correlation : List ( Float, Float ) -> Maybe Float
correlation tupleList =
    case tupleList of
        [] ->
            Nothing

        ( a, b ) :: xs ->
            let
                { meanA, meanB, varA, varB, length } =
                    correlationHelp1 xs (a ^ 2) a (b ^ 2) b 1
            in
            covarianceHelp2 xs meanA meanB ((a - meanA) * (b - meanB))
                / (length * sqrt varA * sqrt varB)
                |> Just


correlationHelp1 remaining squaredA sumA squaredB sumB length =
    case remaining of
        [] ->
            { meanA = sumA / length
            , meanB = sumB / length
            , varA = squaredA / length - (sumA / length) ^ 2
            , varB = squaredB / length - (sumB / length) ^ 2
            , length = length
            }

        ( a, b ) :: xs ->
            correlationHelp1 xs
                (squaredA + a ^ 2)
                (sumA + a)
                (squaredB + b ^ 2)
                (sumB + b)
                (length + 1)


{-| R2 is the square of the correlation coefficient

    > Stat.r2[(1,2),(4,8),(5,10)] == Just 1.00
    > Stat.r2[(10,0),(40,-30),(50,-32)] == Just 0.97

-}
r2 : List ( Float, Float ) -> Maybe Float
r2 tupleList =
    case correlation tupleList of
        Just c ->
            Just (c * c)

        _ ->
            Nothing


{-| Linear regression finds the line that best fits the given points. The method used here is the [simple linear regression](https://en.wikipedia.org/wiki/Linear_regression).
The tuple returned is `(alpha, beta)` where `y = alpha + beta * x`

    > Stat.linearRegression[(1,3),(4,9),(5,11)] == Just (1,2) -- 3 = 1 + 2 * 1

-}
linearRegression : List ( Float, Float ) -> Maybe ( Float, Float )
linearRegression tupleList =
    case tupleList of
        ( a, b ) :: ((_ :: _) as xs) ->
            let
                { meanX, meanY, varX, length } =
                    linearRegressionHelp xs (a ^ 2) a b 1

                cov : Float
                cov =
                    covarianceHelp2 xs meanX meanY ((a - meanX) * (b - meanY))
                        / length
            in
            if varX /= 0 then
                Just ( meanY - (cov / varX) * meanX, cov / varX )

            else
                Nothing

        _ ->
            Nothing


linearRegressionHelp :
    List ( Float, Float )
    -> Float
    -> Float
    -> Float
    -> Float
    -> { meanX : Float, meanY : Float, varX : Float, length : Float }
linearRegressionHelp remaining squaredX sumX sumY length =
    case remaining of
        [] ->
            { meanX = sumX / length
            , meanY = sumY / length
            , varX = squaredX / length - (sumX / length) ^ 2
            , length = length
            }

        ( a, b ) :: xs ->
            linearRegressionHelp xs
                (squaredX + a ^ 2)
                (sumX + a)
                (sumY + b)
                (length + 1)


{-| Returns a function that looks like this: `y = alpha + beta * x`.
This may come in handy when generating points on the regreesion line.

    > Stat.linearRegression[(1,3),(4,9),(5,11)] == Just (1,2)
    > f = Stat.linearRegressionLine (1,2) == <function> : Float -> Float
    > f 5 == 11

-}
linearRegressionLine : ( Float, Float ) -> Float -> Float
linearRegressionLine ( alpha, beta ) x =
    alpha + beta * x
