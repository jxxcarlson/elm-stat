module CsvData exposing (get, getColumnAsFloats, getColumnAsStrings, getPointList, intelligentGet)

{-| The aim of this library is to provide functions for extracting csv data from
strings, and for extracting columns of data and list of points from csv data. Asuume
that you already have valid Csv data. Then

       getColumnAsFloats k csv

will extract a list of floating point numbers from column `k` of the data `csv`.
In the same vein,

       getPointList i j csv

will return a list of points using the data from columns i and column j.

Getting csv data out of a string would be trivial if all data followed
reasnoable conventions, as in the case of the data below:

    Global Land and Ocean Temperature Anomalies
    January-December 1880-2016
    Units: Degrees Celsius
    Base Period: 1901-2000
    Year,Value
    1880,-0.12
    1881,-0.07
    ...
    2015,0.91
    2016,0.95

In this example there are four lines of header text, distinguished by the
fact that they contain no commans. At the fifth line, csv data begins,
giving names to the columns. The rest is good data: pairs of numbers.
The function `intelligentGet sep str` tries its best to return good
csv data from the string `str` using the separator `sep`, typically `","`.
It returns a tuple `(Maybe Csv, Maybe String)`. If succesful, the return
value is `(Just csv, Just header)`, where `csv : Csv` is good data and
`header` is the header as discussed above. More details on this below.

-}

import Csv exposing (Csv)
import List.Extra
import Maybe.Extra
import Stat exposing (Data, Point)


type alias DataState =
    { headerStatus : HeaderStatus
    , sep : String
    , columns : Int
    }


type HeaderStatus
    = HeaderPresent
    | HeaderMissing
    | HeaderUndetermined


{-| Return a Csv value for the string after
filtering out obvious bad fields (no commaa)
-}
get : String -> Maybe Csv
get str =
    Just <| Csv.parse <| filter str


{-| Find the DataState of the string `str` using
a given separator, e.g., "," and attempt to Return
a pair `(Just csvData, Just headerString)`. To do this,
first find the `DataState` of the input string. If it is
Nothing, representing invalid data, return `(Nothing, Nothing)`.
Otherwise, return `(Just csvData, Just headerString)`
Notice that if the data consists of a single column, it
assumed to be a time series and so is augmented by prepending
a column `1,2,3, ...` using `makeSeries`.
-}
intelligentGet : String -> String -> ( Maybe Csv, Maybe String )
intelligentGet sep str =
    case dataState sep str of
        Nothing ->
            ( Nothing, Nothing )

        Just dataState_ ->
            if dataState_.columns == 1 then
                ( makeSeries str, Nothing )

            else
                ( get str, Just <| getHeader str )


{-| Turn data of the form "23.1\\n61.5\\n8.4" into
data of the form "n,value\\n1,23.1\\n2,61.5\\n3,8.4".
-}
makeSeries : String -> Maybe Csv
makeSeries str =
    let
        str2 =
            String.lines str
                |> List.indexedMap (\n x -> String.fromInt (n + 1) ++ "," ++ x)
                |> String.join "\n"
    in
    get <| "n,value\n" ++ str2


{-| Return Nothing if the string does not
meet the criteria to be a Csv file. Otherwise
return Just a record describing what kind
of Csv file it is.
-}
dataState : String -> String -> Maybe DataState
dataState sep str =
    let
        spectrum_ =
            spectrum sep str
    in
    case List.length spectrum_ of
        0 ->
            Nothing

        1 ->
            let
                columns =
                    (List.head spectrum_ |> Maybe.withDefault 0) + 1
            in
            case columns of
                1 ->
                    Just { headerStatus = HeaderUndetermined, sep = sep, columns = columns }

                _ ->
                    Just { headerStatus = HeaderMissing, sep = sep, columns = columns }

        2 ->
            let
                lo =
                    List.Extra.getAt 0 spectrum_ |> Maybe.withDefault 0

                columns =
                    (List.Extra.getAt 1 spectrum_ |> Maybe.withDefault 0) + 1
            in
            if lo == 0 && columns > 0 then
                Just { headerStatus = HeaderPresent, sep = sep, columns = columns }

            else
                Nothing

        _ ->
            Nothing


{-| Return the "comma spectrum" of a string.
Example:

> examine "," csv2
> [0,1] : List Int
> examine "," csv3
> [2] : List Int

A string whose spectrum has length 1 or 2
is "good". If it has length 1, all lines
have the same number of separators. If it
has length two, and has the form [0,n] where
n > 0, then it likely consists of a header
followed by good data with n+1 records per line.

-}
spectrum : String -> String -> List Int
spectrum sep str =
    str
        |> String.lines
        |> List.map (String.indices sep)
        |> List.map List.length
        |> List.Extra.unique
        |> List.sort


{-| filter str returns a string representing potential
csv records: a string of parts separated by "\\n" where
each part contains at least one commna. Veru crude!
-}
filter : String -> String
filter str =
    str
        |> String.lines
        |> List.filter (\x -> String.contains "," x)
        |> String.join "\n"


{-| Get the header of a CSV file: the part
before the data begins. Ih the string
"These are points on a sqaure\\nx,y\\n0,0\\n1,0\\n1,1,\\n0,1".
the header is "These are points on a square".
The next line, "x,y" gives the column headings, which are different.
-}
getHeader : String -> String
getHeader str =
    str
        |> String.lines
        |> List.filter (\x -> not (String.contains "," x))
        |> String.join "\n"


{-| Return column k of Csv data.
-}
getColumnAsStrings : Int -> Csv -> List String
getColumnAsStrings k csv =
    List.map (List.Extra.getAt k) csv.records |> Maybe.Extra.values


{-| Return column k of Csv data, converted to Floats
-}
getColumnAsFloats : Int -> Csv -> List Float
getColumnAsFloats k csv =
    List.map (Maybe.andThen String.toFloat << List.Extra.getAt k) csv.records
        |> Maybe.Extra.values


{-| Extract columns i and j from Csv data and return
the corresponding list of points.
-}
getPointList : Int -> Int -> Csv -> Data
getPointList i j csv =
    let
        xs =
            getColumnAsFloats i csv

        ys =
            getColumnAsFloats j csv
    in
    List.map2 Point xs ys
