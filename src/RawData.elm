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
        , get3
        , get4
        , dataInfo
        , dataSpectrum2
        , filterOutAlphaAt
        , indexOfLastNonNumericalFieldAt
        , indexOfLastNonNumericalField
        , drop
        , getDataAndHeaderUsingColumn
        , getDataAndHeader
        , delimiterProfile
        , delimiter
        , dataType
        )

{-| The RawData module provides utilitis for parsing raw data files
(strings) in which records are separated by spaces, tabs, or commas.
The main function, currently named `get4` operates as follows. Let

    str == "This data describes a square\nx y\n0 0\n1 0\n1 1\n0 1\n"

be some data. Then

    get4 str ==
      Just {
          columnHeaders = ["x","y"]
        , metadata = ["This data describes a square"]
        , rawData = [["0","0"],["1","0"],["1","1"],["0","1"]]
     }

Note what happend: (1) the function computed the probable DataType, in
this case SpaceDelmited; (2) It extracted the metadta preceding the
actual data; (3) it found the column headers; (4) it extracted the
data as a list of records, each of which is a list of strings of a
fixed common size. The type is

    get4 : Data -> DataPackage,

where

    type alias RawData = List Record,

    type alias Record = List String,

and

    type alias DataPacket =
        { metadata : List String
        , columnHeaders : List String
        , rawData : RawData
        }

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
import Csv


type alias DataPacket =
    { metadata : List String
    , columnHeaders : List String
    , rawData : RawData
    }


type alias RawData =
    List Record


type alias Record =
    List String


type DataType
    = SpaceDelimited
    | TabDelimited
    | CommaDelimited


type alias DelimiterStatistics =
    { spaces : Int
    , commas : Int
    , tabs : Int
    }


delimiter : String -> Char
delimiter str =
    case dataType str of
        SpaceDelimited ->
            ' '

        TabDelimited ->
            '\t'

        CommaDelimited ->
            ','


dataType : String -> DataType
dataType str =
    let
        p =
            delimiterProfile str
    in
        if p.tabs > p.spaces && p.tabs > p.spaces then
            TabDelimited
        else if p.commas > p.spaces && p.commas > p.tabs then
            CommaDelimited
        else
            SpaceDelimited


delimiterProfile : String -> DelimiterStatistics
delimiterProfile str =
    { spaces = List.length <| String.indices " " str
    , commas = List.length <| String.indices "," str
    , tabs = List.length <| String.indices "\t" str
    }


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
    if sepChar == ',' then
        Csv.parse str |> (\csv -> [ csv.headers ] ++ csv.records)
    else
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
        |> List.filter
            (\rec -> (Maybe.andThen leadingCharIsAlpha (List.Extra.getAt k rec)) == Just False)


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


{-| get3 makes intelligent guesses, If it is successful

> get3 spaceTest
> Just (["x","y"],[["0","0"],["1","0"],["1","1"],["0","1"]])

-}
get3 : String -> Maybe ( Record, RawData )
get3 str =
    get2 (delimiter str) str |> getDataAndHeader


get4 : String -> Maybe DataPacket
get4 str =
    case get3 str of
        Nothing ->
            Nothing

        Just ( headers, rawData_ ) ->
            let
                k =
                    1 + List.length rawData_

                allRecords =
                    get (delimiter str) str

                n =
                    List.length allRecords
            in
                Just
                    { metadata = List.take (n - k) allRecords |> List.map (String.join " ")
                    , columnHeaders = headers
                    , rawData = rawData_
                    }


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


getDataAndHeader : RawData -> Maybe ( Record, RawData )
getDataAndHeader rawData_ =
    case indexOfLastNonNumericalField rawData_ of
        Nothing ->
            Nothing

        Just k ->
            combineMaybe ( List.Extra.getAt k rawData_, Just (drop (k + 1) rawData_) )


getDataAndHeaderUsingColumn : Int -> RawData -> Maybe ( Record, RawData )
getDataAndHeaderUsingColumn j rawData_ =
    case indexOfLastNonNumericalFieldAt j rawData_ of
        Nothing ->
            Nothing

        Just k ->
            combineMaybe ( List.Extra.getAt k rawData_, Just (drop (k + 1) rawData_) )


indexOfLastNonNumericalFieldAt : Int -> RawData -> Maybe Int
indexOfLastNonNumericalFieldAt k rawData_ =
    column k rawData_
        |> Maybe.withDefault []
        |> List.indexedMap (\i s -> ( i, String.toFloat s ))
        |> List.filter (\( x, y ) -> y == Nothing)
        |> List.reverse
        |> List.head
        |> Maybe.map Tuple.first


indexOfLastNonNumericalField : RawData -> Maybe Int
indexOfLastNonNumericalField rawData_ =
    case Maybe.map List.length (List.Extra.getAt 0 rawData_) of
        Nothing ->
            Nothing

        Just numberOfFields ->
            List.range 0 (numberOfFields - 1)
                |> List.map (\k -> indexOfLastNonNumericalFieldAt k rawData_)
                |> Maybe.Extra.combine
                |> Maybe.map List.maximum
                |> Maybe.Extra.join


combineMaybe : ( Maybe a, Maybe b ) -> Maybe ( a, b )
combineMaybe ( x, y ) =
    case ( x, y ) of
        ( Just a, Just b ) ->
            Just ( a, b )

        ( _, _ ) ->
            Nothing
