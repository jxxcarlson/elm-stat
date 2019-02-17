module RawData
    exposing
        ( RawData
        , get
        , getColumn
        )

{-| The RawData module exposes two functions

    get : String -> Maybe RawData

and

    getData : Int -> Int -> RawData -> Maybe Data

The first intelligently extracts a data table,
column headers, and metadata from a string
representing data in one of several formats --
csv, tab-delimited, or space-delimited. With
the second, one can extract a list of Points
in the xy plane from a data table.

A RawData value is a record of the following form:

    type alias RawData =
        { metadata : List String
        , columnHeaders : List String
        , data : Table
        }

Here is how one can construct such a record from actual deta:

    > get SampleData.temperature
         Just {
             columnHeaders = ["Year","Value"]
           , metadata = ["Global Land and Ocean Temperature Anomalies"
                         ,"January-December 1880-2016"
                         ,"Units: Degrees Celsius"
                        ]
           , rawData = [["1880","-0.12"],["1881","-0.07"],["1882","-0.08"]
                        ["1883","-0.15"], ...

To extract a list of points from the raw data, one proceeds as in the next example:

    > get D.temperature |> Maybe.andThen (dataFromRawData 0 1)

@docs RawData, get, getData, getColumn

-}

import Parser exposing (..)
import Utility
import Types exposing (Point, Data)
import Csv
import Stat


{-| A RawData value consists of metadata, columnHeaders,
and data. The first two are lists of strings, while the
last is a list of records, where a record is a lest of
strings.
-}
type alias RawData =
    { metadata : List String
    , columnHeaders : List String
    , data : Table
    }


type alias Table =
    List Record


type alias Record =
    List String


type DataFormat
    = SpaceDelimited
    | TabDelimited
    | CommaDelimited


type alias DelimiterStatistics =
    { spaces : Int
    , commas : Int
    , tabs : Int
    }


{-| get makes intelligent guesses, If it is successful

> get spaceTest
> Just (["x","y"],[["0","0"],["1","0"],["1","1"],["0","1"]])

The function `get` accomplishes its work as follows. (1) it inspects the
data and makes a guess about is format:

    type DataFormat
        = SpaceDelimited
        | TabDelimited
        | CommaDelimited

Here CommaDelimited means Csv. (2) With this guess in hand the string
is parsed into a Table value, i.e., a list of records. As is, it is
usually in an unsatisfactory state. (3) Another informed guess is made
about the the number of fields in the actual data, i.e., the number of
columns of data. (4) The Table is filtered, removing rows that have
too few or too many fields. (5) It is assumed that the real data is numerical,
and that it starts on row k. An informed guess is made about the value of k,
and the rows 1 through k - 1 are discarded. The actual data is now in hand,
albeit as a table of strings. There is also enough information hand to extract
the column headers -- the row of non-data just before the data starts, and
the other metadata, the rows before the column headers .

The process just described is not fail-safe, but is it works quite frequently.
One sometimes has to fix the data with a text editor.

-}
get : String -> Maybe RawData
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
                    , data = clearData
                    }


{-| getColumn i table extracts a list of Floats from
a table (List of Records) by extracting column i of the
Table, transforming the result to lists of floats.
-}
getColumn : Int -> Table -> Maybe (List Float)
getColumn k rawData_ =
    rawData_
        |> List.map (Utility.listGetAt k)
        |> List.map (Maybe.andThen String.toFloat)
        |> Utility.maybeCombine


delimiter : String -> Char
delimiter str =
    case dataType str of
        SpaceDelimited ->
            ' '

        TabDelimited ->
            '\t'

        CommaDelimited ->
            ','


{-| Examine the input string to see what is the
data format.
-}
dataType : String -> DataFormat
dataType dataString =
    let
        p =
            delimiterProfile dataString
    in
        if p.tabs > p.spaces && p.tabs > p.spaces then
            TabDelimited
        else if p.commas > p.spaces && p.commas > p.tabs then
            CommaDelimited
        else
            SpaceDelimited


{-| Return a record giving the numbeor of times the
characters ' ', ',', and '\t' appear in the file.
-}
delimiterProfile : String -> DelimiterStatistics
delimiterProfile dataString =
    { spaces = List.length <| String.indices " " dataString
    , commas = List.length <| String.indices "," dataString
    , tabs = List.length <| String.indices "\t" dataString
    }


{-| get1 is the lowest level function called by `get`.

To test:

    dd =
        get1 ' ' SampleData.temperature

-}
get1 : Char -> String -> Table
get1 sepChar str =
    if sepChar == ',' then
        Csv.parse str |> (\csv -> [ csv.headers ] ++ csv.records)
    else
        case run (rawData (field sepChar)) str of
            Ok data_ ->
                data_

            Err _ ->
                []


column : Int -> Table -> Maybe (List String)
column k rawData_ =
    rawData_
        |> List.map (\rec -> Utility.listGetAt k rec)
        |> Utility.maybeCombine


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
get2 : Char -> String -> ( List String, Table )
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


{-| dd = getRawData D.temperature

> spectrum2 dd
> [0,2,2,2,2,2,2,..,2,2,2,2,2,3,3,6]

> mode sp
> Just (2,140)

So records of length 2 occur most frequenty and thererfore most likely
represent good rawData.

-}
spectrum2 : Table -> List Int
spectrum2 data_ =
    data_
        |> List.map List.length
        |> List.sort


{-| Drop the first k rows of the raw data
-}
drop : Int -> Table -> Table
drop k rawData_ =
    List.drop k rawData_


getDataAndHeader : Table -> Maybe ( Record, Table )
getDataAndHeader rawData_ =
    case indexOfLastNonNumericalField rawData_ of
        Nothing ->
            Nothing

        Just k ->
            combineMaybe ( Utility.listGetAt k rawData_, Just (drop (k + 1) rawData_) )


indexOfLastNonNumericalField : Table -> Maybe Int
indexOfLastNonNumericalField rawData_ =
    case Maybe.map List.length (Utility.listGetAt 0 rawData_) of
        Nothing ->
            Nothing

        Just numberOfFields ->
            List.range 0 (numberOfFields - 1)
                |> List.map (\k -> indexOfLastNonNumericalFieldAt k rawData_)
                |> Utility.maybeCombine
                |> Maybe.map List.maximum
                |> Utility.maybeJoin


indexOfLastNonNumericalFieldAt : Int -> Table -> Maybe Int
indexOfLastNonNumericalFieldAt k rawData_ =
    column k rawData_
        |> Maybe.withDefault []
        |> List.indexedMap (\i s -> ( i, String.toFloat s ))
        |> List.filter (\( x, y ) -> y == Nothing)
        |> List.reverse
        |> List.head
        |> Maybe.map Tuple.first



--
-- Parsers
--


{-| Test rawData:

d1 = "a b c\nd e f\n"
d2 = "a\tb\tc\nd\t\t\te\tf\n"

> run (rawData (field ' ')) d1
> Ok [["a","b","c"],["d","e","f"]]

> run (rawData (field '\t')) d2
> Ok [["a","b","c"],["d","e","f"]]

-}
rawData : Parser String -> Parser Table
rawData fieldParser =
    loop [] (rawDataGofer fieldParser)


rawDataGofer : Parser String -> Table -> Parser (Step Table Table)
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



--
-- General helpers
--


combineMaybe : ( Maybe a, Maybe b ) -> Maybe ( a, b )
combineMaybe ( x, y ) =
    case ( x, y ) of
        ( Just a, Just b ) ->
            Just ( a, b )

        ( _, _ ) ->
            Nothing
