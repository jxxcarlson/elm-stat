module DataParser
    exposing
        ( Data
        , Record
        , field
        , nextString
        , nextString2
        , record
        )

import Parser exposing (..)
import Parser.Extras exposing (many)


type alias Record =
    List String


type alias Data =
    List Record


record : Parser (List String)
record =
    loop [] recordGofer


recordGofer : List String -> Parser (Step (List String) (List String))
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



--
-- EXPERIMENTS
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
