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
        , get1
        , get2
        , get
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

{-| The main function in the RawData module is

    get : String -> DataPacket,

where

    type alias DataPacket =
        { metadata : List String
        , columnHeaders : List String
        , rawData : RawData
        }

is a record holding metadata (whatever the author wrote),
the names of column headers of the data, and the acrual
data as columns of strings. Here is an example:

    > get DataSamples.temperature
         Just { columnHeaders = ["Year","Value"]
           , metadata = ["Global Land and Ocean Temperature Anomalies"
                         ,"January-December 1880-2016"
                         ,"Units: Degrees Celsius"
                        ]
           , rawData = [["1880","-0.12"],["1881","-0.07"],["1882","-0.08"],["1883","-0.15"],

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
        get1 ' ' DataSamples.temperature

-}
get1 : Char -> String -> RawData
get1 sepChar str =
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
    case (get1 recordParser) str of
        [] ->
            Nothing

        data_ ->
            Stat.mode (spectrum2 data_)


dataSpectrum2 recordParser str =
    case (get1 recordParser) str of
        [] ->
            []

        data_ ->
            spectrum2 data_


{-| get2 attempts to extract well-formed data
from the input string. By well-formed, we mean
that each record has the same number of fields.
To do this, one first finds the raw data using
`get1`. Then the `Stat.mode (spectrum2 rawdata)`
is computed. If succesulf, we obtain
`Just (m, n)`, where `m` is the number of fields
in the most commonly occuring record and `n` is
tne number of records of that shape. Finally,
a filter is applied, allowing only records with
`m` fields to pass through.
-}
get2 : Char -> String -> ( List String, RawData )
get2 sepChar str =
    case get1 sepChar str of
        [] ->
            ( [], [] )

        data_ ->
            case (Stat.mode (spectrum2 data_)) of
                Nothing ->
                    ( [], [] )

                Just ( numberOfFields, numberOfRecords ) ->
                    let
                        goodData =
                            data_ |> List.filter (\rec -> List.length rec == numberOfFields)

                        n =
                            List.length data_

                        k =
                            List.length goodData

                        headerData =
                            List.take (n - k) data_ |> List.map (String.join " ")
                    in
                        ( headerData, goodData )


{-| get makes intelligent guesses, If it is successful

> get spaceTest
> Just (["x","y"],[["0","0"],["1","0"],["1","1"],["0","1"]])

-}
get : String -> Maybe DataPacket
get str =
    let
        ( metadata, goodData ) =
            get2 (delimiter str) str
    in
        case getDataAndHeader goodData of
            Nothing ->
                Nothing

            Just ( columnHeaders, clearData ) ->
                Just
                    { metadata = metadata
                    , columnHeaders = columnHeaders
                    , rawData = clearData
                    }


{-| spectrum list = sorted list of the lengths of the
the sublists in a value of type `RawData = List (List String)`.
A "good" list is one whose spectrum is of length one,
e.g., `[n]`. In that case it is an n-column list:
every rwo consists of n elements.

spectrum [["1.2","-4.5"],["5.6","7.9"]] == [2]
spectrum [["1.2","-4.5"],["5.6"]] == [1,2]

An example of data that needs cleanig before use:

> dd = get1 ' ' D.temperature
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
