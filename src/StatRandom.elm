module StatRandom exposing (generateList, generateMatrix, harmonicMean, mean, median, normal, standardNormal)

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


generateList : Int -> Generator a -> Generator (List a)
generateList cols distribution =
    Random.list cols distribution


generateMatrix : Int -> Int -> Generator a -> Generator (List (List a))
generateMatrix rows cols distribution =
    Random.list rows (Random.list cols distribution)


mean : List Float -> Float
mean list =
    List.foldl (+) 0.0 list / toFloat (List.length list)


harmonicMean : List Float -> Float
harmonicMean list =
    toFloat (List.length list) / List.sum (List.map (\x -> x ^ -1) list)


medianSplit : Int -> Int -> List comparable -> Maybe comparable
medianSplit before after list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            let
                ( smaller, larger ) =
                    List.partition ((>) first) rest

                ( beforeLen, afterLen ) =
                    ( before + List.length smaller, after + List.length larger )

                ( left, right, remains ) =
                    case compare beforeLen afterLen of
                        GT ->
                            ( before, 1 + afterLen, smaller )

                        LT ->
                            ( 1 + beforeLen, after, larger )

                        EQ ->
                            ( 1 + beforeLen, afterLen, [] )
            in
            if remains == [] then
                Just first

            else
                medianSplit left right remains


{-| Returns a median element.
This implementation uses the [quickselect](https://en.wikipedia.org/wiki/Quickselect)
algorithm, and thus has an average time complexity of O(N),
and a worst-case complexity of O(N^2) (when the list is sorted).
If there is a single median element, then it is guarenteed to be returned

> > > median [1,2,3]
> > > Just 2
> > > It works with any sortable type.
> > > That is with ints, floats, chars, strings, lists, and tuples.
> > > median ["Boris", "Arthur", "Adolf", "Jack", "Sarah"]
> > > Just "Boris"
> > > median [(0,1), (0,2), (1,0)]
> > > Just (0,2)
> > > If there is no single median element, it will return an element that most
> > > equally splits the list.
> > > median [1,1,8,1,1]
> > > Just 1
> > > median [1,2,3,4]
> > > Just 3
> > > If the given list is empty, it will return `Nothing`.
> > > median []
> > > Nothing

-}
median : List comparable -> Maybe comparable
median =
    medianSplit 0 0
