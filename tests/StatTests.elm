module StatTests exposing (correlationTest, covarianceTest, geometricMeanTest, harmonicMeanTest, linearRegressionTest, meanAbsoluteDeviationTest, meanFuzzTest, medianAbsoluteDeviationTest, medianFuzzTest, modeTest, rootMeanSquareFuzzTest, skewnessTest, standardDeviationFuzzTest, varianceTest, weightedMeanTest, zScoreTest, zScoresTest)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (..)
import Stat
import Test exposing (..)


meanFuzzTest : Test
meanFuzzTest =
    describe "mean fuzz test"
        [ fuzz (list float) "generating a list of floats" <|
            \floatList ->
                case Stat.mean floatList of
                    Nothing ->
                        Expect.equal floatList []

                    Just avg ->
                        Expect.all
                            [ Expect.atLeast (List.minimum floatList |> Maybe.withDefault 0)
                            , Expect.atMost (List.maximum floatList |> Maybe.withDefault 0)
                            ]
                            avg
        ]


weightedMeanTest : Test
weightedMeanTest =
    let
        xs =
            [ ( 2, 5 ), ( 8, 10 ) ]

        emp =
            []

        ys =
            [ ( 1, 13 ), ( 2, 14 ), ( 3, 15 ), ( 4, 16 ), ( 5, 17 ), ( 6, 18 ) ]
    in
    Test.describe "weighted mean tests"
        [ test "test the example"
            (\_ -> Expect.within (Absolute 0.000000001) 9 (Stat.weightedMean xs |> Maybe.withDefault 0))
        , test "empty list"
            (\_ -> Expect.equal (Stat.weightedMean emp) Nothing)
        , test "long "
            (\_ -> Expect.within (Absolute 0.01) 16.33 (Stat.weightedMean ys |> Maybe.withDefault 0))
        ]


harmonicMeanTest : Test
harmonicMeanTest =
    let
        xs =
            [ 1, 2, 4, 5 ]

        emp =
            []

        ys =
            [ 12, 4, 12, 45, 232, 23, 11, 473, 1, 2, 3, 4, 5, 89, 99 ]
    in
    Test.describe "harmonic mean test"
        [ test "test the example"
            (\_ -> Expect.within (Absolute 0.01) 2.05 (Stat.harmonicMean xs |> Maybe.withDefault 0))
        , test "empty list"
            (\_ -> Expect.equal (Stat.harmonicMean emp) Nothing)
        , test "long "
            (\_ -> Expect.within (Absolute 0.01) 5.2 (Stat.harmonicMean ys |> Maybe.withDefault 0))
        ]


geometricMeanTest : Test
geometricMeanTest =
    let
        xs =
            [ 1, 2.7, 5.9 ]

        emp =
            []

        ys =
            [ 12, 4, 12, 45, 232, 23, 11, 473, 1, 2, 3, 4, 5, 89, 99 ]
    in
    Test.describe "geomteric mean test"
        [ test "test the example"
            (\_ -> Expect.within (Absolute 0.01) 2.51 (Stat.geometricMean xs |> Maybe.withDefault 0))
        , test "empty list"
            (\_ -> Expect.equal (Stat.geometricMean emp) Nothing)
        , test "long "
            (\_ -> Expect.within (Absolute 0.01) 15.56 (Stat.geometricMean ys |> Maybe.withDefault 0))
        ]


modeTest : Test
modeTest =
    let
        xs =
            [ 1, 5, 2, 2, 2, 2, 5, 3, 1 ]

        emp =
            []

        ys =
            [ "red", "green", "red", "blue", "blue", "red" ]
    in
    Test.describe "mode tests"
        [ test "test the example"
            (\_ -> Expect.equal (Just ( 2, 4 )) (Stat.mode xs))
        , test "empty list"
            (\_ -> Expect.equal (Stat.mode emp) Nothing)
        , test "test the example 2"
            (\_ -> Expect.equal (Just ( "red", 3 )) (Stat.mode ys))
        ]


medianFuzzTest : Test
medianFuzzTest =
    describe "median fuzz test"
        [ fuzz (list float) "generating a list of floats" <|
            \floatList ->
                floatList
                    |> Stat.median
                    |> Maybe.withDefault 0
                    |> Expect.all
                        [ Expect.atLeast (List.minimum floatList |> Maybe.withDefault 0)
                        , Expect.atMost (List.maximum floatList |> Maybe.withDefault 0)
                        ]
        ]


rootMeanSquareFuzzTest : Test
rootMeanSquareFuzzTest =
    describe "root mean square fuzz test, rms is equal to mean^2 + stddev^2"
        [ fuzz (list (floatRange -100 100)) "generating a list of floats, hat to limit the range because of float rounding" <|
            \floatList ->
                let
                    mean =
                        Stat.mean floatList |> Maybe.withDefault 0 |> pwr

                    stdev =
                        Stat.standardDeviation floatList |> Maybe.withDefault 0 |> pwr
                in
                floatList
                    |> Stat.rootMeanSquare
                    |> Maybe.withDefault 0
                    |> pwr
                    |> Expect.within (Absolute 0.01) (mean + stdev)
        ]


skewnessTest : Test
skewnessTest =
    let
        xs =
            [ 1, 10.5, 20 ]

        ys =
            [ 1, 2, 3, 10 ]

        zs =
            [ 1, 30, 30, 30 ]
    in
    Test.describe "skewness tests"
        [ test "test the example 1"
            (\_ -> Expect.equal (Just 0) (Stat.skewness xs))
        , test "test the example 2"
            (\_ -> Expect.within (Absolute 0.01) 1.01 (Stat.skewness ys |> Maybe.withDefault 0))
        , test "test the example 3"
            (\_ -> Expect.within (Absolute 0.01) -1.15 (Stat.skewness zs |> Maybe.withDefault 0))
        ]


varianceTest : Test
varianceTest =
    let
        xs =
            [ 1, 1, 1 ]

        ys =
            [ 1, 2, 3, 4, 5 ]

        emp =
            []
    in
    Test.describe "variance tests"
        [ test "test for the same valuee"
            (\_ -> Expect.equal (Just 0) (Stat.variance xs))
        , test "test the example"
            (\_ -> Expect.within (Absolute 0.01) 2 (Stat.variance ys |> Maybe.withDefault 0))
        , test "test for empty list"
            (\_ -> Expect.equal Nothing (Stat.variance emp))
        ]


standardDeviationFuzzTest : Test
standardDeviationFuzzTest =
    describe "standard deviation fuzz test"
        [ fuzz (list float) "generating a list of floats" <|
            \floatList ->
                let
                    var =
                        Stat.variance floatList |> Maybe.withDefault 0 |> sqrt
                in
                floatList
                    |> Stat.standardDeviation
                    |> Maybe.withDefault 0
                    |> Expect.within (Absolute 0.01) var
        ]


meanAbsoluteDeviationTest : Test
meanAbsoluteDeviationTest =
    let
        xs =
            [ 1, 2, 5, 4 ]

        ys =
            [ 1, 2, 4 ]

        emp =
            []
    in
    Test.describe "mean absolute deviation tests"
        [ test "test the example 1"
            (\_ -> Expect.within (Absolute 0.01) 1.5 (Stat.meanAbsoluteDeviation xs |> Maybe.withDefault 0))
        , test "test the example 2"
            (\_ -> Expect.within (Absolute 0.01) 1.11 (Stat.meanAbsoluteDeviation ys |> Maybe.withDefault 0))
        , test "test empty"
            (\_ -> Expect.equal Nothing (Stat.meanAbsoluteDeviation emp))
        ]


medianAbsoluteDeviationTest : Test
medianAbsoluteDeviationTest =
    let
        xs =
            [ 1, 2, 5, 4 ]

        ys =
            [ 1, 2, 4 ]

        emp =
            []
    in
    Test.describe "median absolute deviation tests"
        [ test "test the example 1"
            (\_ -> Expect.within (Absolute 0.01) 1.5 (Stat.medianAbsoluteDeviation xs |> Maybe.withDefault 0))
        , test "test the example 2"
            (\_ -> Expect.within (Absolute 0.01) 1.0 (Stat.medianAbsoluteDeviation ys |> Maybe.withDefault 0))
        , test "test empty"
            (\_ -> Expect.equal Nothing (Stat.medianAbsoluteDeviation emp))
        ]


zScoreTest : Test
zScoreTest =
    Test.describe "z score tests"
        [ test "test the example"
            (\_ -> Expect.within (Absolute 0.01) -1.26 (Stat.zScore 1 3 1.58))
        ]


zScoresTest : Test
zScoresTest =
    Test.describe "z scores tests"
        [ test "test the example"
            (\_ -> Expect.within (Absolute 0.01) -1.26 (Stat.zScores [ 1, 2, 4, 5 ] |> Maybe.withDefault [] |> List.head |> Maybe.withDefault 0))
        ]


covarianceTest : Test
covarianceTest =
    let
        xs =
            [ ( 1, 2 ), ( 4, 8 ), ( 5, 10 ) ]
    in
    Test.describe "covariance tests"
        [ test "test the example"
            (\_ -> Expect.within (Absolute 0.01) 5.77 (Stat.covariance xs |> Maybe.withDefault 0))
        ]


correlationTest : Test
correlationTest =
    let
        xs =
            [ ( 1, 2 ), ( 4, 8 ), ( 5, 10 ) ]

        ys =
            [ ( 10, 0 ), ( 40, -30 ), ( 50, -32 ) ]
    in
    Test.describe "correlation tests"
        [ test "test the example 1"
            (\_ -> Expect.within (Absolute 0.01) 1 (Stat.correlation xs |> Maybe.withDefault 0))
        , test "test the example 2"
            (\_ -> Expect.within (Absolute 0.01) -0.98 (Stat.correlation ys |> Maybe.withDefault 0))
        ]


linearRegressionTest : Test
linearRegressionTest =
    let
        xs =
            [ ( 1, 3 ), ( 4, 9 ), ( 5, 11 ) ]

        em =
            []

        ys =
            [ ( 1, 1 ), ( 1, 1 ), ( 1, 1 ) ]
    in
    Test.describe "linear regression tests"
        [ test "test the example 1"
            (\_ -> Expect.within (Absolute 0.01) 1.0 (Stat.linearRegression xs |> Maybe.withDefault ( 0, 0 ) |> Tuple.first))
        , test "test the example 2"
            (\_ -> Expect.within (Absolute 0.01) 2.0 (Stat.linearRegression xs |> Maybe.withDefault ( 0, 0 ) |> Tuple.second))
        , test "test empty"
            (\_ -> Expect.equal Nothing (Stat.linearRegression em))
        , test "test zero variance"
            (\_ -> Expect.equal Nothing (Stat.linearRegression ys))
        ]


pwr : Float -> Float
pwr num =
    num ^ 2
