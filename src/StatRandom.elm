module StatRandom exposing (bernoulliBool, bernoulliInt, generateList, generateMatrix, normal, standardNormal)

import Random exposing (Generator, float, map)


{-| Create a generator of floats that is normally distributed with
given mean and standard deviation.
-}
normal : Float -> Float -> Generator Float
normal mean_ stdDev =
    map (\u -> u * stdDev + mean_) standardNormal


{-| A generator that follows a standard normal distribution (as opposed to
a uniform distribution)
-}
standardNormal : Generator Float
standardNormal =
    Random.map2
        (\u theta -> sqrt (-2 * logBase e (1 - max 0 u)) * cos theta)
        (float 0 1)
        (float 0 (2 * pi))


bernoulliBool : Float -> Generator Bool
bernoulliBool p =
    Random.weighted ( p * 100, True ) [ ( 100 - (p * 100), False ) ]


bernoulliInt : Float -> Generator Int
bernoulliInt p =
    Random.weighted ( p * 100, 1 ) [ ( 100 - (p * 100), 0 ) ]


generateList : Int -> Generator a -> Generator (List a)
generateList cols distribution =
    Random.list cols distribution


generateMatrix : Int -> Int -> Generator a -> Generator (List (List a))
generateMatrix rows cols distribution =
    Random.list rows (Random.list cols distribution)
