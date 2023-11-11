module Benchmarks exposing (main)

import Benchmark exposing (describe, Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Stat


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        numbers : List Float
        numbers =
            List.range 1 1000 |> List.map toFloat 

        weightedNumbers : List (Float, Float)
        weightedNumbers =
            List.foldl (\el (w, acc) -> ( w + 0.05, ( w, el )  :: acc )) (0.05, []) numbers
                |> Tuple.second
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
