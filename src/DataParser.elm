module DataParser
    exposing
        ( Data
        , Record
        , field
        , tabbedField
        , record
        , tabbedRecord
        , data
        , spectrum
        , spectrum2
        , get
        , get2
        , dataInfo
        , dataSpectrum2
        , filterOutAlphaAt
        )

{-| The DataParser module provides utilitis for parsing data files
in which records are separated by spaces, as in the sealevel rise
data of `DataSamples.sealevel`from the NASA Goddard Space Flight Center
Run

    > dataInfo D.sealevel
    Just (12,954) : Maybe ( Int, Int )

on this data. The result tells you that the data proably consists of
954 lines of twelve records each. Now Run

    > get2 D.sealevel
    [["HDR","For","information","on","how","the","data","were","generate","please",
    "refer","to:"],["HDR","7","standard","deviation","of","GMSL","(GIA","not",
    "applied)","variation","estimate","(mm)"],["0","11","1993.0115260","466462",
    "337277.00","-37.24","92.66","-37.02","-37.24","92.66","-37.02","-37.52"],
    ["0","12","1993.0386920","460889","334037.31","-40.35","95.39","-38.20",
    "-40.34","95.39","-38.19","-38.05"], ...

The `get2` function has done a pretty good job of separating data and metadata,
but some spurions lines remain. They can be removed by the following technique:

    get2 D.sealevel |> filterOutAlphaAt 10

the `filterOutAlphaAt 10` function filters out (supresses) all records
which have an alphabetical string in column 10. In this discussion,
columns are numbered beginning with 0.

-}

import Parser exposing (..)
import List.Extra
import Stat


type alias Record =
    List String


type alias Data =
    List Record


{-| run (data record) "1.2 -4.5\n5.6 7.9\n"
Ok [["1.2","-4.5"],["5.6","7.9"]]
-}
data : Parser Record -> Parser Data
data recordParser =
    loop [] (dataGofer recordParser)


dataGofer : Parser Record -> Data -> Parser (Step Data Data)
dataGofer recordParser revFields =
    oneOf
        [ succeed (\s -> Loop (s :: revFields))
            |= recordParser
            |. symbol "\n"
        , succeed ()
            |> map (\_ -> Done (List.reverse revFields))
        ]


{-| run record "1.2 -4.5"
Ok ["1.2","-4.5"] : Result (List DeadEnd) Record
-}
record : Parser Record
record =
    loop [] recordGofer


recordGofer : Record -> Parser (Step Record Record)
recordGofer revFields =
    oneOf
        [ succeed (\s -> Loop (s :: revFields))
            |= field
        , succeed ()
            |> map (\_ -> Done (List.reverse revFields))
        ]


{-| A field as defined here begins with an alphanumeric
character or '-' and is surrounded by spaces.
-}
field : Parser String
field =
    (succeed String.slice
        |= getOffset
        |. chompWhile (\c -> c == ' ')
        |. chompIf (\c -> c /= ' ' && c /= '\n')
        |. chompWhile (\c -> c /= ' ' && c /= '\n')
        |. chompWhile (\c -> c == ' ')
        |= getOffset
        |= getSource
    )
        |> map String.trim


{-| run record "1.2 -4.5"
Ok ["1.2","-4.5"] : Result (List DeadEnd) Record
-}
tabbedRecord : Parser Record
tabbedRecord =
    loop [] tabbedGofer


tabbedGofer : Record -> Parser (Step Record Record)
tabbedGofer revFields =
    oneOf
        [ succeed (\s -> Loop (s :: revFields))
            |= tabbedField
        , succeed (\s -> Loop (s :: revFields))
            |= lastTabbedField
        , succeed ()
            |> map (\_ -> Done (List.reverse revFields))
        ]


{-| A field as defined here begins with an alphanumeric
character or '-' and is surrounded by spaces.
-}
tabbedField : Parser String
tabbedField =
    (succeed String.slice
        |= getOffset
        |. chompIf (\c -> c /= '\t' && c /= '\n')
        |. chompWhile (\c -> c /= '\t' && c /= '\n')
        |. chompIf (\c -> c == '\t')
        |= getOffset
        |= getSource
    )
        |> map String.trim


lastTabbedField : Parser String
lastTabbedField =
    (succeed String.slice
        |= getOffset
        |. chompIf (\c -> c /= '\t' && c /= '\n')
        |. chompWhile (\c -> c /= '\t' && c /= '\n')
        -- |. chompIf (\c -> c == '\n')
        |= getOffset
        |= getSource
    )


get : Parser Record -> String -> Data
get recordParser str =
    case run (data recordParser) str of
        Ok data_ ->
            data_

        Err _ ->
            []


filterOutAlphaAt : Int -> Data -> Data
filterOutAlphaAt k data_ =
    data_
        |> List.filter (\rec -> (Maybe.andThen leadingCharIsAlpha (List.Extra.getAt k rec)) == Just False)


leadingCharIsAlpha : String -> Maybe Bool
leadingCharIsAlpha str =
    String.uncons str
        |> Maybe.map Tuple.first
        |> Maybe.map Char.isAlpha


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


get2 : Parser Record -> String -> Data
get2 recordParser str =
    case (get recordParser) str of
        [] ->
            []

        data_ ->
            case (Stat.mode (spectrum2 data_)) of
                Nothing ->
                    []

                Just ( numberOfFields, numberOfRecords ) ->
                    data_ |> List.filter (\rec -> List.length rec == numberOfFields)


{-| spectrum list = sorted list of the lengths of the
the sublists in a value of type `Data = List (List String)`.
A "good" list is one whose spectrum is of length one,
e.g., `[n]`. In that case it is an n-column list:
every rwo consists of n elements.

spectrum [["1.2","-4.5"],["5.6","7.9"]] == [2]
spectrum [["1.2","-4.5"],["5.6"]] == [1,2]

-}
spectrum : Data -> List Int
spectrum data_ =
    data_
        |> List.map List.length
        |> List.Extra.unique
        |> List.sort


{-| dd = getData D.temperature

> spectrum2 dd
> [0,2,2,2,2,2,2,..,2,2,2,2,2,3,3,6]

> mode sp
> Just (2,140)

So records of length 2 occur most frequenty and thererfore most likely
represent good data.

-}
spectrum2 : Data -> List Int
spectrum2 data_ =
    data_
        |> List.map List.length
        |> List.sort



--
-- recordInfo : Data -> Maybe (comparable, Int)
-- recordInfo data =
--
--
-- IRRELEVANT EXPERIMENTS
--


{-| run (field |> andThen (\s -> nextString s)) "a b c"
-}
nextString : String -> Parser (List String)
nextString s =
    field |> map (\t -> [ s, t ])


{-| run (field |> map (\s -> [s]) |> andThen (\s -> nextString2 s) |> andThen (\s -> nextString2 s)) "a b c"

> Ok ["c","b","a"]

-}
nextString2 : List String -> Parser (List String)
nextString2 stringList =
    field |> map (\t -> t :: stringList)
