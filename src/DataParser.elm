module DataParser
    exposing
        ( Data
        , Record
        , field
        , record
        , data
        , spectrum
        , spectrum2
        )

import Parser exposing (..)
import List.Extra


type alias Record =
    List String


type alias Data =
    List Record


{-| run data "1.2 -4.5\n5.6 7.9\n"
Ok [["1.2","-4.5"],["5.6","7.9"]]
-}
data : Parser Data
data =
    loop [] dataGofer


dataGofer : Data -> Parser (Step Data Data)
dataGofer revFields =
    oneOf
        [ succeed (\s -> Loop (s :: revFields))
            |= record
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
        |. chompIf (\c -> Char.isAlphaNum c || c == '-')
        |. chompWhile (\c -> c /= ' ' && c /= '\n')
        |. chompWhile (\c -> c == ' ')
        |= getOffset
        |= getSource
    )
        |> map String.trim


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


spectrum2 : Data -> List Int
spectrum2 data_ =
    data_
        |> List.map List.length
        |> List.sort



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
