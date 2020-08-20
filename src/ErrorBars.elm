module ErrorBars exposing (ErrorBar, mean, normal, maxmin)

{-| The `ErrorBars` module provides functions for
drawing graphs with errors bars. Suppose that
we have dome data:

    > data = SampleData.errorBarData |> Data.fromString 0 1

Then

    > ErrorBars.mean data

produces a list of ponts that pass through the
mean values of data points with given x value.
For error bars with extreme ponts one standard
deviatoin from the mean, use

    > ErrorBars.normal 1.0 data

For error bars with endpoints at the maximumn
and minimum of the data with given x value, use

    > ErrorBars.maxmin data

@docs ErrorBar, mean, normal, maxmin

-}

import Data exposing (Data, Point, xCoord, yCoord)
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


{-| The type of an error bar containing
(x,y) with extreme points (x,top) and
(x,bottom)
-}
type alias ErrorBar =
    { x : Float
    , y : Float
    , top : Float
    , bottom : Float
    }


type alias ErrorData =
    List ErrorDatum


type alias ErrorDictionary =
    Dict Float (List Float)


{-| Use to compute the y-centroids of the data, as in this example:

    > SampleData.errorBarData |> Data.fromString 0 1 |> ErrorBars.mean
    [{ x = 0, y = 1 },{ x = 1, y = 2 }]

-}
mean : Data -> Data
mean data =
    data
        |> rawStats
        |> List.map meanValue
        |> Utility.maybeValues


{-| Use to compute error bars, as in this example:

    > SampleData.eberrorBarData2 |> Data.fromString 0 1 |> ErrorBars.normal 0.5
      [{ bottom = 0.99667, y = 1, top = 1.003333, x = 0 }
      ,{ bottom = 1.98, y = 2, top = 2.02, x = 1 }]

-}
normal : Float -> Data -> List ErrorBar
normal p data =
    data
        |> rawStats
        |> List.map (errorBar p)
        |> Utility.maybeValues


{-|

    > SampleData.errorBarData |> Data.fromString 0 1 |> ErrorBars.maxmin
      [{ bottom = 0.9, y = 1, top = 1.1, x = 0 }
       ,{ bottom = 1.8, y = 2, top = 2.2, x = 1 }]

-}
maxmin : Data -> List ErrorBar
maxmin data =
    data
        |> rawStats
        |> List.map maxminBar
        |> Utility.maybeValues


errorBar : Float -> ErrorStats -> Maybe ErrorBar
errorBar p es =
    case ( es.mean, es.stdev ) of
        ( Just m, Just s ) ->
            Just
                { x = es.x
                , top = m + p * s
                , y = m
                , bottom = m - p * s
                }

        ( _, _ ) ->
            Nothing


maxminBar : ErrorStats -> Maybe ErrorBar
maxminBar es =
    case ( es.mean, es.min, es.max ) of
        ( Just m, Just a, Just b ) ->
            Just
                { x = es.x
                , top = b
                , y = m
                , bottom = a
                }

        ( _, _, _ ) ->
            Nothing


meanValue : ErrorStats -> Maybe Point
meanValue es =
    case es.mean of
        Nothing ->
            Nothing

        Just y ->
            Just ( es.x, y )


{-|

    > Data.fromString 0 1 SampleData.errorBarData |> ErrorStat.rawStats
       [ { max = Just 1.1, mean = Just 1, min = Just 0.9, stdev = Just 0.0066666666666666706 }
       ,{ max = Just 2.2, mean = Just 2, min = Just 1.8, stdev = Just 0.04000000000000002 }

-}
rawStats : Data -> List ErrorStats
rawStats data =
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
            Just <| List.sum xs / n


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
    case Dict.get (xCoord point) errorDict of
        Nothing ->
            Dict.insert (xCoord point) [ yCoord point ] errorDict

        Just ys ->
            Dict.insert (xCoord point) (yCoord point :: ys) errorDict


{-|

> dd = fromString 0 1 SD.errorBarData

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
            Just <| sqrt <| List.sum squaredDifferences / (n - 1)

        ( _, _ ) ->
            Nothing
