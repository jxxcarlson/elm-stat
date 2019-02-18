module ErrorStat exposing (ErrorDatum, ErrorStats, get, mean, errorBars)

import Types exposing (Point, Data)
import Dict exposing (Dict)
import Utility


type alias ErrorDatum =
    { x : Float
    , ys : List Float
    }


type alias ErrorStats =
    { x : Float
    , mean : Maybe Float
    , min : Maybe Float
    , max : Maybe Float
    , stdev : Maybe Float
    }


type alias ErrorBar =
    { x : Float
    , top : Float
    , mid : Float
    , bottom : Float
    }


type alias ErrorData =
    List ErrorDatum


type alias ErrorDictionary =
    Dict Float (List Float)


errorBar : Float -> ErrorStats -> Maybe ErrorBar
errorBar p es =
    case ( es.mean, es.stdev ) of
        ( Just m, Just s ) ->
            Just
                { x = es.x
                , top = m + p * s
                , mid = m
                , bottom = m - p * s
                }

        ( _, _ ) ->
            Nothing


{-| Use to ompute error bars, as in this example:

    > SampleData.eb2 |> Data.fromString 0 1 |> ErrorStat.get |> ErrorStat.errorBars 0.5
      [{ bottom = 0.99667, mid = 1, top = 1.003333, x = 0 }
      ,{ bottom = 1.98, mid = 2, top = 2.02, x = 1 }]

-}
errorBars : Float -> List ErrorStats -> List ErrorBar
errorBars p ess =
    List.map (errorBar p) ess
        |> Utility.maybeValues


{-| Use to ompute the y-centroids of the data, as in this example:

> SampleData.eb2 |> Data.fromString 0 1 |> ErrorStat.get |> ErrorStat.mean
> [{ x = 0, y = 1 },{ x = 1, y = 2 }]

-}
mean : List ErrorStats -> Data
mean ess =
    ess
        |> List.map meanValue
        |> Utility.maybeValues


meanValue : ErrorStats -> Maybe Point
meanValue es =
    case es.mean of
        Nothing ->
            Nothing

        Just y ->
            Just { x = es.x, y = y }


{-|

    > Data.fromString 0 1 SampleData.eb2 |> ErrorStat.get
       [ { max = Just 1.1, mean = Just 1, min = Just 0.9, stdev = Just 0.0066666666666666706 }
       ,{ max = Just 2.2, mean = Just 2, min = Just 1.8, stdev = Just 0.04000000000000002 }
-}
get : Data -> List ErrorStats
get data =
    data
        |> getErrorData2
        |> List.map errorStatsFromErrorDatum


rawMean : List Float -> Maybe Float
rawMean xs =
    let
        n_ =
            List.length xs

        n =
            toFloat n_
    in
        case n == 0 of
            True ->
                Nothing

            False ->
                Just <| (List.sum xs) / n


errorStatsFromErrorDatum : ErrorDatum -> ErrorStats
errorStatsFromErrorDatum d =
    let
        n_ =
            List.length d.ys

        n =
            toFloat n_

        m =
            rawMean d.ys
    in
        { x = d.x
        , mean = m
        , min = List.minimum d.ys
        , max = List.maximum d.ys
        , stdev = rawStdev m d.ys
        }


emptyErrorDictionary : ErrorDictionary
emptyErrorDictionary =
    Dict.empty


insertInErrorDict : Point -> ErrorDictionary -> ErrorDictionary
insertInErrorDict point errorDict =
    case Dict.get point.x errorDict of
        Nothing ->
            Dict.insert point.x [ point.y ] errorDict

        Just ys ->
            Dict.insert point.x (point.y :: ys) errorDict


{-|

> dd = fromString 0 1 SD.eb2
-}
getErrorData2 : Data -> List ErrorDatum
getErrorData2 data =
    data
        |> List.foldl (\p dict -> insertInErrorDict p dict) emptyErrorDictionary
        |> Dict.toList
        |> List.map (\( x, ys ) -> { x = x, ys = ys })


rawStdev : Maybe Float -> List Float -> Maybe Float
rawStdev rawMean_ xs =
    let
        n_ =
            List.length xs

        n =
            toFloat n_
    in
        case ( rawMean_, n_ > 1 ) of
            ( Just m, True ) ->
                let
                    square x =
                        x * x

                    squaredDifferences =
                        List.map (\x -> square (x - m)) xs
                in
                    Just <| List.sum squaredDifferences / (n - 1)

            ( _, _ ) ->
                Nothing