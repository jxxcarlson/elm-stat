module StatRandom exposing (bernoulliBool, bernoulliInt, generateList, generateMatrix, normal, standardNormal, binomial, poisson, geometric, exponential, beta)

{-| The goal of this module is to provide commonly used probability distributions.

@docs bernoulliBool, bernoulliInt, generateList, generateMatrix, normal, standardNormal, binomial, poisson, geometric, exponential, beta

-}

import Random exposing (Generator, float, map)
import Utility exposing (combination, sterling)


{-| Create a generator of floats that is normally distributed with
given mean and standard deviation.
-}
normal : Float -> Float -> Generator Float
normal mean_ stdDev =
    map (\u -> u * stdDev + mean_) standardNormal


{-| A generator that follows a standard normal distribution.
-}
standardNormal : Generator Float
standardNormal =
    Random.map2
        (\u theta -> sqrt (-2 * logBase e (1 - max 0 u)) * cos theta)
        (float 0 1)
        (float 0 (2 * pi))


{-| A generator that follows the exponential distribution with the given `lambda`.
-}
exponential : Float -> Generator Float
exponential lambda =
    Random.map (\u -> logBase e (1 - u) / -lambda)
        (float 0 1)


{-| A generator that follows the beta distribution with the given `alpha` and `beta` parameters.
-}
beta : Float -> Float -> Generator Float
beta a b =
    Random.map (\u -> (u ^ (a - 1) * (1 - u) ^ (b - 1)) / (sterling a * sterling b / sterling (a + b)))
        (float 0 1)


{-| A discrete generator that follows the bernoulli distribution and returns `true` or `false` with the given probability.
-}
bernoulliBool : Float -> Generator Bool
bernoulliBool p =
    Random.weighted ( p * 100, True ) [ ( 100 - (p * 100), False ) ]


{-| A discrete generator that follows the bernoulli distribution and returns `1` or `0` with the given probability.
-}
bernoulliInt : Float -> Generator Int
bernoulliInt p =
    Random.weighted ( p * 100, 1 ) [ ( 100 - (p * 100), 0 ) ]


{-| A discrete generator that follows the binomial distribution with the given probability for `n` number of trials.
-}
binomial : Float -> Int -> Generator Int
binomial p n =
    let
        options =
            List.range 0 n

        dis =
            List.map (\x -> bin n x p) options
    in
    case dis of
        x :: xs ->
            Random.weighted x xs

        [] ->
            bernoulliInt p


bin : Int -> Int -> Float -> ( Float, Int )
bin n k p =
    let
        kk =
            toFloat k

        comb =
            combination n k |> toFloat

        nk =
            n - k |> toFloat
    in
    Tuple.pair (comb * p ^ kk * (1 - p) ^ nk) k


{-| A discrete generator that follows the poisson distribution with the given lambda and for `n` number of trials.
-}
poisson : Float -> Int -> Generator Int
poisson lambda n =
    let
        options =
            List.range 0 n

        el =
            e ^ -lambda

        dis =
            List.map (\x -> pois lambda el x) options
    in
    case dis of
        x :: xs ->
            Random.weighted x xs

        [] ->
            bernoulliInt lambda


pois : Float -> Float -> Int -> ( Float, Int )
pois lambda el k =
    let
        kk =
            toFloat k
    in
    Tuple.pair ((lambda ^ kk * el) / toFloat (Utility.factorial k)) k


{-| A discrete generator that follows the geomtric distribution with the given probability for `n` number of trials.
-}
geometric : Float -> Int -> Generator Int
geometric p n =
    let
        options =
            List.range 1 n

        dis =
            List.map (\x -> geo p x) options
    in
    case dis of
        [] ->
            Random.constant 0

        _ :: [] ->
            Random.constant 0

        x :: xs ->
            Random.weighted x xs


geo : Float -> Int -> ( Float, Int )
geo p k =
    let
        kk =
            toFloat k
    in
    Tuple.pair ((1 - p) ^ (kk - 1) * p) k


{-| Generates a list, same as Random.list
-}
generateList : Int -> Generator a -> Generator (List a)
generateList cols distribution =
    Random.list cols distribution


{-| Generates a matrix with the given form.
-}
generateMatrix : Int -> Int -> Generator a -> Generator (List (List a))
generateMatrix rows cols distribution =
    Random.list rows (Random.list cols distribution)
