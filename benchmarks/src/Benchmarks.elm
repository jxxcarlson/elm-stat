module Benchmarks exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict
import Stat
import Utility exposing (buildTable, combineTuple)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        numbers : List Float
        numbers =
            List.range 1 1000 |> List.map toFloat

        weightedNumbers : List ( Float, Float )
        weightedNumbers =
            List.foldl (\el ( w, acc ) -> ( w + 0.05, ( w, el ) :: acc )) ( 0.05, [] ) numbers
                |> Tuple.second

        repeatedStrings : List String
        repeatedStrings =
            List.repeat 250 "red"
                ++ List.repeat 251 "blue"
                ++ List.repeat 249 "green"
                ++ List.repeat 250 "yellow"
    in
    describe "elm-stat"
        [ Benchmark.compare "Mean: tail call recursion VS foldl + tuple"
            "New"
            (\() -> Stat.mean numbers)
            "Old"
            (\() -> mean numbers)
        , Benchmark.compare "Weighted mean: single VS multiple traversals"
            "New"
            (\() -> Stat.weightedMean weightedNumbers)
            "Old"
            (\() -> weightedMean weightedNumbers)
        , Benchmark.compare "Harmonic mean: single VS multiple traversals"
            "New"
            (\() -> Stat.harmonicMean numbers)
            "Old"
            (\() -> harmonicMean numbers)
        , Benchmark.compare "Geometric mean: single VS multiple traversals"
            "New"
            (\() -> Stat.geometricMean numbers)
            "Old"
            (\() -> geometricMean numbers)
        , Benchmark.compare "Mode: sorting VS Dict"
            "New"
            (\() -> Stat.mode repeatedStrings)
            "Old"
            (\() -> mode repeatedStrings)
        , Benchmark.compare "Variance: single VS multiple traversals"
            "New"
            (\() -> Stat.variance numbers)
            "Old"
            (\() -> variance numbers)
        , Benchmark.compare "RMS: single VS multiple traversals"
            "New"
            (\() -> Stat.rootMeanSquare numbers)
            "Old"
            (\() -> rootMeanSquare numbers)
        , Benchmark.compare "Mean absolute deviation"
            "New"
            (\() -> Stat.meanAbsoluteDeviation numbers)
            "Old"
            (\() -> meanAbsoluteDeviation numbers)
        , Benchmark.compare "Median absolute deviation"
            "New"
            (\() -> Stat.medianAbsoluteDeviation numbers)
            "Old"
            (\() -> medianAbsoluteDeviation numbers)
        , Benchmark.compare "zScores"
            "New"
            (\() -> Stat.zScores numbers)
            "Old"
            (\() -> zScores numbers)
        , Benchmark.compare "Covariance"
            "New"
            (\() -> Stat.covariance <| List.map2 Tuple.pair numbers numbers)
            "Old"
            (\() -> covariance <| List.map2 Tuple.pair numbers numbers)
        , Benchmark.compare "Correlation"
            "New"
            (\() -> Stat.correlation <| List.map2 Tuple.pair numbers numbers)
            "Old"
            (\() -> correlation <| List.map2 Tuple.pair numbers numbers)
        , Benchmark.compare "Linear Regression"
            "New"
            (\() -> Stat.linearRegression <| List.map2 Tuple.pair numbers numbers)
            "Old"
            (\() -> linearRegression <| List.map2 Tuple.pair numbers numbers)
        ]



-- Old versions


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
    List.foldl (\x y -> Tuple.pair (Tuple.first y + x) (Tuple.second y + 1))
        ( 0, 0 )


weightedMean : List ( Float, Float ) -> Maybe Float
weightedMean tupleList =
    let
        wSum =
            List.map (\t -> Tuple.first t) tupleList |> List.sum
    in
    if wSum == 0 then
        Nothing

    else
        (List.map (\t -> Tuple.first t * Tuple.second t) tupleList |> List.sum)
            / wSum
            |> Just


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


variance : List Float -> Maybe Float
variance list =
    mean list
        |> Maybe.andThen
            (\n ->
                List.map (\x -> (x - n) ^ 2) list
                    |> mean
            )


standardDeviation : List Float -> Maybe Float
standardDeviation =
    variance >> Maybe.map sqrt


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


meanAbsoluteDeviation : List Float -> Maybe Float
meanAbsoluteDeviation list =
    mean list
        |> Maybe.andThen
            (\n ->
                List.map (\x -> abs (x - n)) list |> mean
            )


medianAbsoluteDeviation : List Float -> Maybe Float
medianAbsoluteDeviation list =
    median list
        |> Maybe.andThen
            (\n ->
                List.map (\x -> abs (x - n)) list |> mean
            )


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


zScores : List Float -> Maybe (List Float)
zScores list =
    Maybe.map2 (\avg stdDev -> List.map (\x -> (x - avg) / stdDev) list)
        (mean list)
        (standardDeviation list)


covariance : List ( Float, Float ) -> Maybe Float
covariance tupleList =
    let
        ( xs, ys ) =
            List.unzip tupleList
    in
    Maybe.map2 (\mxs mys -> List.map2 (\x y -> (x - mxs) * (y - mys)) xs ys) (mean xs) (mean ys)
        |> Maybe.andThen mean


correlation : List ( Float, Float ) -> Maybe Float
correlation tupleList =
    let
        ( xs, ys ) =
            List.unzip tupleList
    in
    Maybe.map3 (\cov stdDevXs stdDevYs -> cov / (stdDevXs * stdDevYs)) (covariance tupleList) (standardDeviation xs) (standardDeviation ys)


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
