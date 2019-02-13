module RawData
    exposing
        ( RawData
        , Record
        , rawData
        , record
        , field
        , column
        , spectrum
        , spectrum2
        , get
        , get2
        , dataInfo
        , dataSpectrum2
        , filterOutAlphaAt
        , lastNonNumericalFieldAt
        , drop
        , getDataAndHeaderUsingColumn
        )

{-| The RawData module provides utilitis for parsing raw data files
(srings) in which records are separated by spaces or tabs, as in
`RawDataSamples.sealevel`.

Run

    > dataInfo ' ' D.sealevel
    Just (12,954) : Maybe ( Int, Int )

on this data. The first argument of `dataInfo` is the charecter
which separates fields. The result tells you that the rawData
proably consists of 954 lines of twelve records each. Now Run

    > get2 ' ' D.sealevel
    [["HDR","For","information","on","how","the","rawData","were","generate","please",
    "refer","to:"],["HDR","7","standard","deviation","of","GMSL","(GIA","not",
    "applied)","variation","estimate","(mm)"],["0","11","1993.0115260","466462",
    "337277.00","-37.24","92.66","-37.02","-37.24","92.66","-37.02","-37.52"],
    ["0","12","1993.0386920","460889","334037.31","-40.35","95.39","-38.20",
    "-40.34","95.39","-38.19","-38.05"], ...

The `get2` function has done a pretty good job of separating rawData and metadata,
but some spurions lines remain. They can be removed by the following technique:

    get2 ' ' D.sealevel |> filterOutAlphaAt 10

the `filterOutAlphaAt 10` function filters out (supresses) all records
which have an alphabetical string in column 10. In this discussion,
columns are numbered beginning with 0.

-}

import Parser exposing (..)
import List.Extra
import Maybe.Extra
import Stat


type alias RawData =
    List Record


type alias Record =
    List String


{-| Test rawData:

d1 = "a b c\nd e f\n"
d2 = "a\tb\tc\nd\t\t\te\tf\n"

> run (rawData (field ' ')) d1
> Ok [["a","b","c"],["d","e","f"]]

> run (rawData (field '\t')) d2
> Ok [["a","b","c"],["d","e","f"]]

-}
rawData : Parser String -> Parser RawData
rawData fieldParser =
    loop [] (rawDataGofer fieldParser)


rawDataGofer : Parser String -> RawData -> Parser (Step RawData RawData)
rawDataGofer fieldParser revFields =
    oneOf
        [ succeed (\s -> Loop (s :: revFields))
            |= record fieldParser
            |. symbol "\n"
        , succeed ()
            |> map (\_ -> Done (List.reverse revFields))
        ]


{-| Test rawData:

str1 = "a b c\n"
str2 = "a\tb\tc\n"

> run (record (field ' ')) str1
> Ok ["a","b","c"]

> run (record (field '\t')) str2
> Ok ["a","b","c"]

-}
record : Parser String -> Parser Record
record fieldParser =
    loop [] (recordGofer fieldParser)


recordGofer : Parser String -> Record -> Parser (Step Record Record)
recordGofer fieldParser revFields =
    oneOf
        [ succeed (\s -> Loop (s :: revFields))
            |= fieldParser
        , succeed ()
            |> map (\_ -> Done (List.reverse revFields))
        ]


{-| A field may be preceded by any number of `sepChar
and consists of one or more characters which is neither
a sepChar or a newline.
-}
field : Char -> Parser String
field sepChar =
    (succeed String.slice
        |= getOffset
        |. chompWhile (\c -> c == sepChar)
        |. chompIf (\c -> c /= sepChar && c /= '\n')
        |. chompWhile (\c -> c /= sepChar && c /= '\n')
        |= getOffset
        |= getSource
    )
        |> map String.trim


{-| Try these:

    dd =
        get ' ' DataSamples.temperature

-}
get : Char -> String -> RawData
get sepChar str =
    case run (rawData (field sepChar)) str of
        Ok data_ ->
            data_

        Err _ ->
            []


column : Int -> RawData -> Maybe (List String)
column k rawData_ =
    rawData_
        |> List.map (\rec -> List.Extra.getAt k rec)
        |> Maybe.Extra.combine


{-| Remove all records in the data where the field in column
k is alphabetical.
-}
filterOutAlphaAt : Int -> RawData -> RawData
filterOutAlphaAt k data_ =
    data_
        |> List.filter (\rec -> (Maybe.andThen leadingCharIsAlpha (List.Extra.getAt k rec)) == Just False)


leadingCharIsAlpha : String -> Maybe Bool
leadingCharIsAlpha str =
    String.uncons str
        |> Maybe.map Tuple.first
        |> Maybe.map Char.isAlpha


{-| See the introductory comments.
-}
dataInfo recordParser str =
    case (get recordParser) str of
        [] ->
            Nothing

        data_ ->
            Stat.mode (spectrum2 data_)


dataSpectrum2 recordParser str =
    case (get recordParser) str of
        [] ->
            []

        data_ ->
            spectrum2 data_


{-| get2 attempts to extract well-formed data
from the input string. By well-formed, we mean
that each record has the same number of fields.
To do this, one first finds the raw data using
`get`. Then the `Stat.mode (spectrum2 rawdata)`
is computed. If succesulf, we obtain
`Just (m, n)`, where `m` is the number of fields
in the most commonly occuring record and `n` is
tne number of records of that shape. Finally,
a filter is applied, allowing only records with
`m` fields to pass through.
-}
get2 : Char -> String -> RawData
get2 sepChar str =
    case get sepChar str of
        [] ->
            []

        data_ ->
            case (Stat.mode (spectrum2 data_)) of
                Nothing ->
                    []

                Just ( numberOfFields, numberOfRecords ) ->
                    data_ |> List.filter (\rec -> List.length rec == numberOfFields)


{-| spectrum list = sorted list of the lengths of the
the sublists in a value of type `RawData = List (List String)`.
A "good" list is one whose spectrum is of length one,
e.g., `[n]`. In that case it is an n-column list:
every rwo consists of n elements.

spectrum [["1.2","-4.5"],["5.6","7.9"]] == [2]
spectrum [["1.2","-4.5"],["5.6"]] == [1,2]

An example of data that needs cleanig before use:

> dd = get ' ' D.temperature
> spectrum dd
> [0,2,3,6]

The fact that the spectum consiste of more than
one elemen tis the tipoff: there are records with
0, 2, e, and 6 fields. Applyibg `spectrum2`, we
see that there are many more 2-field records,
so the data must be of this shape.

-}
spectrum : RawData -> List Int
spectrum data_ =
    data_
        |> List.map List.length
        |> List.Extra.unique
        |> List.sort


{-| dd = getRawData D.temperature

> spectrum2 dd
> [0,2,2,2,2,2,2,..,2,2,2,2,2,3,3,6]

> mode sp
> Just (2,140)

So records of length 2 occur most frequenty and thererfore most likely
represent good rawData.

-}
spectrum2 : RawData -> List Int
spectrum2 data_ =
    data_
        |> List.map List.length
        |> List.sort


{-| Drop the first k rows of the raw data
-}
drop : Int -> RawData -> RawData
drop k rawData_ =
    List.drop k rawData_


getDataAndHeaderUsingColumn : Int -> RawData -> Maybe ( Record, RawData )
getDataAndHeaderUsingColumn j rawData_ =
    case lastNonNumericalFieldAt j rawData_ of
        Nothing ->
            Nothing

        Just k ->
            foo ( List.Extra.getAt k rawData_, Just (drop (k + 1) rawData_) )


{-| Use lastNonNumericalFieldAt to find where the good data starts.

    dd = get2 ' ' D.temperature
         [["January-December","1880-2016"],["Missing:","-999"],["Year","Value"]
         ,["1880","-0.12"],["1881","-0.07"],["1882","-0.08"],["1883","-0.15"],...

    > lastNonNumericalFieldAt 0 dd
      Just 2

-}
lastNonNumericalFieldAt : Int -> RawData -> Maybe Int
lastNonNumericalFieldAt k rawData_ =
    column k rawData_
        |> Maybe.withDefault []
        |> List.indexedMap (\i s -> ( i, String.toFloat s ))
        |> List.filter (\( x, y ) -> y == Nothing)
        |> List.reverse
        |> List.head
        |> Maybe.map Tuple.first


foo : ( Maybe a, Maybe b ) -> Maybe ( a, b )
foo ( x, y ) =
    case ( x, y ) of
        ( Just a, Just b ) ->
            Just ( a, b )

        ( _, _ ) ->
            Nothing
