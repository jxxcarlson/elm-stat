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
    in
    describe "elm-stat"
        [ describe "Mean"
            [ Benchmark.compare "Stat.mean VS naive mean"
                "Stat.mean"
                (\() -> Stat.mean numbers)
                "List.sum / List.length"
                (\() -> List.sum numbers / toFloat (List.length numbers))
            , let 
                oldMean : List Float -> Maybe Float
                oldMean list =
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
                    List.foldl 
                        (\x y -> 
                            Tuple.pair (Tuple.first y + x) (Tuple.second y + 1)
                        ) 
                        ( 0, 0 ) 
             in
             Benchmark.compare "Tail call recursion VS foldl + tuple"
                "Tail call"
                (\() -> Stat.mean numbers)
                "Foldl + tuple"
                (\() -> oldMean numbers)
            ]
        ]
