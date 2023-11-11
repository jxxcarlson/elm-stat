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
            List.range 0 1000 |> List.map toFloat 

        weightedNumbers : List (Float, Float)
        weightedNumbers =
            List.foldl (\el (w, acc) -> ( w + 0.05, ( w, el )  :: acc )) (0.05, []) numbers
                |> Tuple.second
    in
    describe "elm-stat"
        [ describe "Mean"
            [ Benchmark.compare "Stat.mean VS naive mean"
                "Stat.mean"
                (\() -> Stat.mean numbers)
                "List.sum / List.length"
                (\() -> List.sum numbers / toFloat (List.length numbers))
            , Benchmark.compare "Tail call recursion VS foldl + tuple"
                "Tail call"
                (\() -> Stat.mean numbers)
                "Foldl + tuple"
                (\() -> mean numbers)
            ]
        , describe "Weighted mean"
            [ Benchmark.compare "Single VS multiple traversals"
                "New"
                (\() -> Stat.weightedMean weightedNumbers)
                "Old"
                (\() -> weightedMean weightedNumbers)
            ]
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
