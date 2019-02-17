module ErrorStat exposing (ErrorDatum, get)

import Types exposing (Point, Data)
import Dict exposing (Dict)


type alias ErrorDatum =
    { x : Float
    , ys : List Float
    }


type alias MaybeErrorStats =
    { mean : Maybe Float
    , min : Maybe Float
    , max : Maybe Float
    , stdev : Maybe Float
    }


{-|

> Data.fromString 0 1 SampleData.eb2 |> ErrorStat.get
-}
get : Data -> List MaybeErrorStats
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


errorStatsFromErrorDatum : ErrorDatum -> MaybeErrorStats
errorStatsFromErrorDatum d =
    let
        n_ =
            List.length d.ys

        n =
            toFloat n_

        m =
            rawMean d.ys
    in
        { mean = m
        , min = List.minimum d.ys
        , max = List.maximum d.ys
        , stdev = rawStdev m d.ys
        }


type alias ErrorData =
    List ErrorDatum


type alias ErrorDictionary =
    Dict Float (List Float)


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



-- |> List.map
-- groupByX : Data -> ErrorData
-- groupByX data =
--     let
--         xs =
--             List.sortBy (\p -> p.x)
--     in
--         xs
