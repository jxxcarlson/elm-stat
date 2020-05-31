module Stat exposing (average, averageDeviation, geometricMean, harmonicMean, mean, meanWithDefault, median, mode, rootMeanSquare, skewness, standardDeviation, variance, weightedMean, zScore, zScores)

import Dict as Dict
import Utility exposing (buildTable, combineTuple)


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


mean : List Float -> Maybe Float
mean list =
    let
        ( sum, len ) =
            List.foldl (\x y -> Tuple.pair (Tuple.first y + x) (Tuple.second y + 1)) ( 0, 0 ) list
    in
    if len == 0 then
        Nothing

    else
        sum / toFloat len |> Just


average : List Float -> Maybe Float
average =
    mean


weightedMean : List ( Float, Float ) -> Maybe Float
weightedMean toupleList =
    let
        wSum =
            List.map (\t -> Tuple.first t) toupleList |> List.sum
    in
    if wSum == 0 then
        Nothing

    else
        (List.map (\t -> Tuple.first t * Tuple.second t) toupleList |> List.sum) / wSum |> Just


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


averageDeviation : List Float -> Maybe Float
averageDeviation list =
    mean list
        |> Maybe.andThen
            (\n ->
                List.map (\x -> abs (x - n)) list |> mean
            )


zScore : Float -> Float -> Float -> Float
zScore x avg stdDev =
    (x - avg) / stdDev


zScores : List Float -> Maybe (List Float)
zScores list =
    Maybe.map2 (\avg stdDev -> List.map (\x -> (x - avg) / stdDev) list) (mean list) (standardDeviation list)
